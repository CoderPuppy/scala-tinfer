package cpup.typeInference

import java.util.UUID

import scala.collection.mutable
import scala.reflect.internal.util.WeakHashSet

class Graph {
	protected val _places = mutable.Set.empty[Place]
	def places: Set[Place] = _places.toSet

	protected val _typs = mutable.Set.empty[Type]
	def typs: Set[Type] = _typs.toSet

	protected val _typIds = new mutable.WeakHashMap[Any, Type]
	def typIds = _typIds.toMap

	protected val _labels = mutable.Set.empty[Label]
	def labels = _labels.toSet

	def unknown = new Place(new Type.Unknown)

	def typ(name: String) = new Place(new Type.Identifier(name))

	sealed trait Type {
		val uuid = UUID.randomUUID()

		protected[typeInference] val _places = mutable.Set.empty[Place]
		def places = _places.toSet

		def merge(other: Type): Option[Type]
		def id: Any

		protected var oldId: Any = null
		def updateId {
			if(id == oldId) return
			_typIds.remove(oldId)
			_typIds(id) = this
			oldId = id
		}
	}
	object Type {
		private var unknownNames = Stream.from(1).map { n =>
			Integer.toString(n, 27).map {
				case c if c >= '0' && c <= '9' => (c + 17).toChar
				case c if c >= 'a' && c <= 'z' => (c - 22).toChar
			}
		}

		class Unknown extends Type {
			val name = unknownNames.head
			unknownNames = unknownNames.tail

			def merge(other: Type) = Some(other)
			def id = this

			override def toString = s"unknown:$name"
		}

		class Identifier(val name: String) extends Type {
			def id = name
			def merge(other: Type) = other match {
				case other: Identifier if other.name == name => Some(this)

				case _ =>
					None
			}

			override def toString = name
		}

		class Construct(val fn: Place, val arg: Place) extends Type {
			fn._dependents += this
			arg._dependents += this

			def id = (fn.typ.id, arg.typ.id)
			def merge(other: Type) = other match {
				case other: Construct =>
					fn ++ other.fn
					arg ++ other.arg
					Some(this)

				case _ =>
					None
			}

			override def toString = s"$fn($arg)"
		}

		class Or(_opts: Set[Type]) extends Type {
			lazy val optsP: Set[Place] = _opts.flatMap {
				case o: Type.Or => o.optsT
				case t => Set(t)
			}.map(new Place(_))

			for(optP <- optsP) optP._dependents += this

			def optsT = optsP.map(_.typ)

			def merge(other: Type) = other match {
				case o: Or =>
					val intersect = optsT.intersect(o.optsT)
					if(intersect.nonEmpty) Some(new Type.Or(intersect)) else None

				case _ =>
					optsT.view.flatMap(_.merge(other)).headOption
			}
			def id = optsT

			override def toString = "(" + optsP.mkString(" || ") + ")"
		}
	}

	class Label(val label: String, val place: Place) {
		val uuid = UUID.randomUUID
	}

	protected val merging = new WeakHashSet[Place]

	class Place(protected[typeInference] var _typ: Type) {
		val uuid = UUID.randomUUID

		protected[typeInference] val _dependents = new WeakHashSet[Type]
		def dependents = _dependents.toSet

		_places += this
		_typ._places += this
		_typs += _typ
		typ.updateId
		def typ = _typ

		dedup

		def ++(other: Place): Place = {
			if(other == this) return this
			if(merging.contains(this)) {
				println("recursive merging", this, other)
				return this
			}
			merging += this

			val oldTyp = _typ

			_typIds.remove(oldTyp.id)
			_typIds.remove(other._typ.id)

			_typ = _typ.merge(other._typ)
				.orElse(other._typ.merge(_typ))
				.getOrElse(throw new RuntimeException(s"cannot merge $this and $other"))

			_typ._places ++= other._typ.places
			_typ._places ++= oldTyp.places

			_typs -= oldTyp
			_typs -= other._typ
			_typs += _typ
			_typIds(_typ.id) = _typ

			for(pl <- _typ._places)
				pl._typ = _typ

			val queue = mutable.Queue.empty[Place]
			queue.enqueue(this)
			queue.enqueue(oldTyp._places.toSeq: _*)
			queue.enqueue(other._typ._places.toSeq: _*)
			for(pl <- queue) {
				pl.typ.updateId
				pl.dedup
				queue.enqueue(pl._dependents.view.flatMap(_.places).toSeq: _*)
			}

			merging -= this

			this
		}

		def dedup {
			for {
				reged <- _typIds.get(_typ.id)
				if _typ != reged
				regedP <- reged._places.headOption
			} {
				regedP ++ this
			}
		}

		protected val _labels = mutable.Set.empty[Label]
		def labels = _labels.toSet
		def label(label: String) = {
			val lbl = new Label(label, this)
			Graph.this._labels += lbl
			_labels += lbl
			this
		}

		def apply(arg: Place) = new Place(new Type.Construct(this, arg))
		def ||(other: Place) = new Place(new Type.Or(Set(_typ, other._typ)))

		override def toString = typ.toString
	}

	val fnTyp = typ("Function").label("fn")

	sealed trait Expr {
		def use: Expr.Use

		def apply(arg: Expr) = Expr.Call(this, arg)

		def force(pl: Place) = Expr.Force(this, pl)

		def label(label: String) = Expr.Label(this, label)
	}
	object Expr {
		trait Use {
			def expr: Expr
			def typ: Place
		}

		case class Undefined(pl: Place) extends Expr {
			object Use extends Expr.Use {
				def expr = Undefined.this
				def typ = pl
			}
			def use = Use
		}

		case class Call(fn: Expr, arg: Expr) extends Expr {
			class Use extends Expr.Use {
				def expr = Call.this

				val fnUse = fn.use
				val argUse = arg.use

				val typ = unknown
				fnUse.typ ++ fnTyp(argUse.typ)(typ)
			}

			def use = new Use
		}

		case class Scope(create: () => Expr.Use) extends Expr {
			override def use = create()
		}

		case class Function(fn: Expr => Expr) extends Expr {
			class Use extends Expr.Use {
				def expr = Function.this

				object Arg extends Expr {
					def parentUse = Use.this

					object Use extends Expr.Use {
						def expr = Arg
						val typ = unknown
					}
					def use = Use
				}

				val res = fn(Arg)

				val typ = fnTyp(Arg.use.typ)(res.use.typ)
			}

			def use = new Use
		}

		case class Label(wrapped: Expr, label: String) extends Expr {
			class Use extends Expr.Use {
				def expr = Label.this
				val wrapped = Label.this.wrapped.use

				wrapped.typ.label(label)
				def typ = wrapped.typ
			}

			override def use = new Use
		}

		case class Force(wrapped: Expr, pl: Place) extends Expr {
			class Use extends Expr.Use {
				def expr = Force.this
				val wrapped = Force.this.wrapped.use

				def typ = wrapped.typ

				typ ++ pl
			}

			override def use = new Use
		}
	}

	def scope(create: () => Expr.Use) = Expr.Scope(create)
	def typed(pl: Place = unknown) = Expr.Undefined(pl)
	def fn(fn: Expr => Expr) = Expr.Function(fn)
}
