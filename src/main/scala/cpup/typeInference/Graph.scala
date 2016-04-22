package cpup.typeInference

import java.util.UUID

import scala.collection.mutable
import scala.reflect.internal.util.WeakHashSet

class Graph {
	protected val _places = mutable.Set.empty[Place]
	def places: Set[Place] = _places.toSet
	protected val _typs = new WeakHashSet[Type]()
	def typs: Set[Type] = _typs.toSet

	def unknown = new Place(new Type.Unknown)

	def typ(name: String) = new Place(new Type.Identifier(name))

	sealed trait Type {
		val uuid = UUID.randomUUID
		_typs += this
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
			for(pl <- oldTyp.places)
				pl._typ = _typ
			for(pl <- other.typ.places)
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

		override def toString = typ.toString
	}
}
