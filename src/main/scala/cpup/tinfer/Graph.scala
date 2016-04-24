package cpup.tinfer

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

	protected val _typLabels = mutable.Set.empty[Type.Label]
	def typLabels = _typLabels.toSet

	def unknown = new Place(new Type.Unknown)

	def typ(name: String) = new Place(new Type.Identifier(name))

	sealed trait Type {
		val uuid = UUID.randomUUID()

		protected[tinfer] val _places = mutable.Set.empty[Place]
		def places = _places.toSet

		def merge(other: Type): Option[Type]
		def id: Any

		protected var oldId: Any = null
		def updateId {
			if(id == oldId) return
			_typIds.remove(oldId)
			_typIds.getOrElseUpdate(id, this)
			oldId = id

			_places.head ++ _typIds(id).places.head
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

		class Label(val label: String, val place: Place) {
			val uuid = UUID.randomUUID
		}
	}

	protected val merging = new WeakHashSet[Place]

	class Place(protected[tinfer] var _typ: Type) {
		val uuid = UUID.randomUUID

		protected[tinfer] val _dependents = new WeakHashSet[Type]
		def dependents = _dependents.toSet

		_places += this
		_typ._places += this
		_typs += _typ
		typ.updateId
		def typ = _typ

		def ++(other: Place): Place = {
			if(other == this) return this
			if(merging.contains(this)) {
//				println("recursive merging", this, other)
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
//				pl.dedup
				queue.enqueue(pl._dependents.view.flatMap(_.places).toSeq: _*)
			}

			merging -= this

			this
		}

//		def dedup {
//			for {
//				reged <- _typIds.get(_typ.id)
//				if _typ != reged
//				regedP <- reged._places.headOption
//			} {
//				regedP ++ this
//			}
//		}

		protected val _labels = mutable.Set.empty[Type.Label]
		def labels = _labels.toSet
		def label(label: String) = {
			val lbl = new Type.Label(label, this)
			Graph.this._typLabels += lbl
			_labels += lbl
			this
		}

		def apply(arg: Place) = new Place(new Type.Construct(this, arg))

		override def toString = typ.toString
	}

	val fnTyp = typ("Function").label("fn")

	protected val _exprs = new WeakHashSet[Expr]
	def exprs = _exprs.toSet

	protected val _exprUses = new WeakHashSet[Expr.Use]
	def exprUses = _exprUses.toSet

	protected val _exprLabels = mutable.Set.empty[Expr.Label]
	def exprLabels = _exprLabels.toSet

	sealed trait Expr {
		val uuid = UUID.randomUUID()
		_exprs += this

		type U <: Expr.Use
		def uses: Set[U]
		def use: U

		def apply(arg: Expr) = Expr.Call(this, arg)

		def force(pl: Place) = Expr.Force(this, pl)

		def label(label: String) = {
			val lbl = new Expr.Label(label, this)
			_exprLabels += lbl
			this
		}
	}
	object Expr {
		trait Use {
			val uuid = UUID.randomUUID()
			_exprUses += this

			def expr: Expr
			def typ: Place
		}

		case class Unimplemented(pl: Place) extends Expr {
			type U = Use.type
			object Use extends Expr.Use {
				def expr = Unimplemented.this
				def typ = pl
			}
			def uses = Set(Use)
			def use = Use
		}

		case class Call(fn: Expr, arg: Expr) extends Expr {
			type U = Use

			class Use extends Expr.Use {
				def expr = Call.this

				val fnUse = fn.use
				val argUse = arg.use

				val typ = unknown
				fnUse.typ ++ fnTyp(argUse.typ)(typ)
			}

			protected val _uses = new WeakHashSet[Use]
			def uses = _uses.toSet
			def use = {
				val use = new Use
				_uses += use
				use
			}
		}

		case class Scope(create: () => Expr.Use) extends Expr {
			type U = Expr.Use
			protected val _uses = new WeakHashSet[Expr.Use]
			def uses = _uses.toSet
			def use = {
				val use = create()
				_uses += use
				use
			}
		}

		case class Function(fn: Expr => Expr) extends Expr {
			trait Arg extends Expr {
				def parentUse: Use
			}

			type U = Use
			class Use extends Expr.Use {
				def expr = Function.this

				object Arg extends Function.this.Arg {
					def parentUse = Use.this

					type U = Use.type
					object Use extends Expr.Use {
						def expr = Arg
						val typ = unknown
					}
					def uses = Set(Use)
					def use = Use
				}

				val res = fn(Arg)
				val resUse = res.use

				val typ = fnTyp(Arg.use.typ)(resUse.typ)
			}

			protected val _uses = new WeakHashSet[Use]
			def uses = _uses.toSet
			def use = {
				val use = new Use
				_uses += use
				use
			}
		}

		case class Force(wrapped: Expr, pl: Place) extends Expr {
			type U = Use
			class Use extends Expr.Use {
				def expr = Force.this
				val wrapped = Force.this.wrapped.use

				def typ = wrapped.typ

				typ ++ pl
			}

			protected val _uses = new WeakHashSet[Use]
			def uses = _uses.toSet
			def use = {
				val use = new Use
				_uses += use
				use
			}
		}

		class Isolate extends Expr {
			protected var _expr: Option[Expr] = None
			def expr = _expr
			protected var _exprUse: Option[Expr.Use] = None
			def exprUse = _exprUse
			protected var _pattern: Option[Place] = None
			def pattern = _pattern
			protected val _uses = new WeakHashSet[Use]

			type U = Use
			class Use extends Expr.Use {
				def expr = Isolate.this

				val typ = unknown

				if(_pattern.isDefined)
					build()

				def build() {
					val map = mutable.Map.empty[Type, Place]
					def build(pat: Place): Place = map.getOrElseUpdate(pat.typ, pat.typ match {
						case _: Type.Unknown => unknown
						case _: Type.Identifier => pat
						case c: Type.Construct => build(c.fn)(build(c.arg))
					})
					typ ++ build(_pattern.get)
				}
			}

			def build(__expr: Expr) = {
				_expr = Some(__expr)
				_exprUse = Some(__expr.use)
				_pattern = Some(_exprUse.get.typ)
				for(use <- _uses) use.build()
				this
			}

			def uses = _uses.toSet
			def use = {
				val use = new Use
				_uses += use
				use
			}

			override def toString = s"Isolate(${_expr.map(_.toString).getOrElse("")})"
		}

		class Label(val label: String, val expr: Expr) {
			val uuid = UUID.randomUUID
		}
	}

	def scope(create: () => Expr.Use) = Expr.Scope(create)
	def unimplemented = Expr.Unimplemented(unknown)
	def uimpl = unimplemented
	def fn(fn: Expr => Expr) = Expr.Function(fn)
	def isolate = new Expr.Isolate
}
