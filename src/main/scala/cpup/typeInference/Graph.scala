package cpup.typeInference

import java.util.UUID

import scala.collection.mutable
import scala.reflect.internal.util.WeakHashSet

class Graph {
	protected val _places = mutable.Set.empty[Place.Use]
	def places: Set[Place] = _places.toSet
	protected val _typs = new WeakHashSet[Type]()
	def typs: Set[Type] = _typs.toSet

	def unknown = new Place.Impl(None).use

	def typ(name: String, args: Place*) = new Place.Impl(Some(new Type.Construct(name, args: _*))).use

	sealed trait Type {
		val uuid = UUID.randomUUID
		_typs += this

		def merge(other: Type): Type
	}
	object Type {
		class Construct(val name: String, val args: Place*) extends Type {
			def merge(other: Type) = {
				other match {
					case other: Construct if other.name == name && other.args.size == args.size =>
						for((sa, oa) <- args.view.zip(other.args))
							sa ++ oa
				}
				this
			}
		}
	}

	sealed trait Place {
		def uuid: UUID
		def uses: Set[Place.Use]
		def use = new Place.Use(this)
		def ++(other: Place): Place
		def typ: Option[Type]
	}

	object Place {
		class Impl(var typ: Option[Type]) extends Place {
			val uuid = UUID.randomUUID

			protected[typeInference] val _uses = new WeakHashSet[Place.Use]()
			def uses = _uses.toSet

			def ++(other: Place) = {
				typ = (typ, other.typ) match {
					case (Some(a), Some(b)) => Some(a.merge(b))
					case (Some(a), None) => Some(a)
					case (None, Some(b)) => Some(b)
					case (None, None) => None
				}
				_uses ++= other.uses
				for(u <- other.uses)
					u._place = this
				this
			}
		}

		class Use(protected[typeInference] var _place: Place) extends Place {
			_places += this
			def place = _place

			_place match {
				case pl: Place.Impl =>
					pl._uses += this
				case _ =>
			}

			override def uses = _place.uses
			override def typ = _place.typ
			override def uuid = _place.uuid
			override def ++(other: Place) = _place ++ other

			val individualUUID = UUID.randomUUID

			protected var _label: Option[String] = None
			def label = _label
			def label(label: String): Place = {
				_label = Some(label)
				this
			}
		}
	}
}
