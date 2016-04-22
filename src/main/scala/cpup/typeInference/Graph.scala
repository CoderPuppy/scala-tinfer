package cpup.typeInference

import java.util.UUID

import scala.collection.mutable

class Graph {
	protected val _places = mutable.Set.empty[Place]
	def places: Set[Place] = _places.toSet
	protected val _typs = new mutable.WeakHashMap[Type, UUID]()
	def typs: Set[Type] = _typs.keySet.toSet

	def unknown = new Place(new Type.Unknown)

	def typ(name: String) = new Place(new Type.Identifier(name))

	sealed trait Type {
		if(!_typs.contains(this)) _typs(this) = UUID.randomUUID
		def uuid = _typs(this)

		protected[typeInference] val _places = mutable.Set.empty[Place]
		def places = _places.toSet

		def merge(other: Type): Type
	}
	object Type {
		private var unknownNames = Stream.from(1).flatMap { n =>
			('A' to 'Z').map { c =>
				c.toString * n
			}
		}

		class Unknown extends Type {
			val name = unknownNames.head
			unknownNames = unknownNames.tail

			override def merge(other: Type) = other

			override def toString = s"unknown:$name"
		}

		case class Identifier(name: String) extends Type {
			override def merge(other: Type) = other match {
				case other: Identifier if other.name == name => this

				case _ =>
					throw new RuntimeException(s"cannot merge $this and $other")
			}

			override def toString = name
		}

		case class Construct(fn: Place, arg: Place) extends Type {
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

	private var unlabeledNames = Stream.from(1).flatMap { n =>
		('A' to 'Z').map { c =>
			c.toString * n
		}
	}

	class Place(protected[typeInference] var _typ: Type) {
		_places += this
		_typ._places += this
		def typ = _typ

		val name = unlabeledNames.head
		unlabeledNames = unlabeledNames.tail

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

			this
		}
		val uuid = UUID.randomUUID

		protected var _label: Option[String] = None
		def label = _label
		def label(label: String): Place = {
			_label = Some(label)
			this
		}

		def apply(arg: Place) = new Place(new Type.Construct(this, arg))

		override def toString = s"$typ@${label.getOrElse(name)}"
	}
}
