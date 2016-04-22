package cpup.typeInference

import java.util.UUID

import scala.collection.mutable
import scala.reflect.internal.util.WeakHashSet

class Graph {
	protected val _places = mutable.Set.empty[Place]
	def places: Set[Place] = _places.toSet
	protected val _typs = new WeakHashSet[Type]
	def typs: Set[Type] = _typs.toSet
	protected val _labels = mutable.Set.empty[Label]
	def labels = _labels.toSet

	def unknown = new Place(new Type.Unknown)

	def typ(name: String) = new Place(new Type.Identifier(name))

	sealed trait Type {
		val uuid = UUID.randomUUID()
		_typs += this

		protected[typeInference] val _places = mutable.Set.empty[Place]
		def places = _places.toSet

		def merge(other: Type): Type
	}
	object Type {
		private var unknownNames = Stream.from(1).map { n =>
			Integer.toString(n, 26).toCharArray.map {
				case c if c >= '0' && c <= '9' => (c + 17).toChar
				case c if c >= 'a' && c <= 'z' => (c - 32).toChar
			}.mkString
		}

		class Unknown extends Type {
			val name = unknownNames.head
			unknownNames = unknownNames.tail

			override def merge(other: Type) = other

			override def toString = s"unknown:$name"
		}

		class Identifier(val name: String) extends Type {
			override def merge(other: Type) = other match {
				case other: Identifier if other.name == name => this

				case _ =>
					throw new RuntimeException(s"cannot merge $this and $other")
			}

			override def toString = name
		}

		class Construct(val fn: Place, val arg: Place) extends Type {
			def merge(other: Type) = {
				other match {
					case other: Construct =>
						fn ++ other.fn
						arg ++ other.arg

					case _ =>
						throw new RuntimeException(s"cannot merge $this and $other")
				}
				this
			}

			override def toString = s"$fn($arg)"
		}
	}

	class Label(val label: String, val place: Place) {
		val uuid = UUID.randomUUID
	}

	private var unlabeledNames = Stream.from(1).map { n =>
		Integer.toString(n, 26).toCharArray.map {
			case c if c >= '0' && c <= '9' => (c + 17).toChar
			case c if c >= 'a' && c <= 'z' => (c - 32).toChar
		}.mkString
	}

	class Place(protected[typeInference] var _typ: Type) {
		_places += this
		_typ._places += this
		def typ = _typ

		def ++(other: Place) = {
			val oldTyp = _typ
			_typ = other.typ match {
				case _: Type.Unknown => oldTyp
				case otyp => oldTyp.merge(otyp)
			}

			_typ._places ++= other.typ.places
			_typ._places ++= oldTyp.places

			for(pl <- _typ._places)
				pl._typ = _typ

//			_typs -= oldTyp
//			_typs -= other.typ
//			_typs += _typ

			this
		}
		val uuid = UUID.randomUUID

		protected val _labels = mutable.Set.empty[Label]
		def labels = _labels.toSet
		def label(label: String) = {
			val lbl = new Label(label, this)
			Graph.this._labels += lbl
			_labels += lbl
			this
		}

		def apply(arg: Place) = new Place(new Type.Construct(this, arg))

		override def toString = typ.toString
	}

	val fnTyp = typ("Function").label("fn")

	sealed trait Expr {
		def use: Expr.Use

		def apply(arg: Expr) = Expr.Call(this, arg)

		def label(label: String) = Expr.Label(this, label)
	}
	object Expr {
		trait Use {
			def expr: Expr
			def typ: Place

			def force(_typ: Place) = {
				typ ++ _typ
				this
			}
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

				val typ = wrapped.typ.label(label)
			}

			override def use = new Use
		}
	}

	def scope(create: () => Expr.Use) = Expr.Scope(create)
	def undefined = Expr.Undefined(unknown)
	def fn(fn: Expr => Expr) = Expr.Function(fn)
}
