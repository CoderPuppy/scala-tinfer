package cpup.typeInference

import java.util.regex.Pattern

class DOTPrinter(g: Graph) {
	private var unknownNames = Stream.from(1).flatMap { n =>
		('A' to 'Z').map { c =>
			c.toString * n
		}
	}

	val out = new StringBuilder

	for(pl <- g.places if pl.isInstanceOf[g.Place.Use]) {
		print(pl.asInstanceOf[g.Place.Use])

		var pl_ : g.Place = pl
		while(pl_.isInstanceOf[g.Place.Use]) {
			pl_ = pl_.asInstanceOf[g.Place.Use].place
		}
		print(pl_.asInstanceOf[g.Place.Impl])
	}

	for(typ <- g.typs)
		print(typ)

	def print(pl: g.Place.Use) {
		pl.label match {
			case Some(label) =>
				out ++= "\""
				out ++= pl.individualUUID.toString
				out ++= "\" [label=\"label: "
				out ++= label.replace("\\", "\\\\").replace("\"", "\\\"")
				out ++= "\"]\n"
				out ++= "\""
				out ++= pl.individualUUID.toString
				out ++= "\" -> \""
				out ++= pl.uuid.toString
				out ++= "\"\n"
			case _ =>
		}
	}

	def print(pl: g.Place.Impl) {
		out ++= "\""
		out ++= pl.uuid.toString
		out ++= "\";\n"
		for(typ <- pl.typ) {
			out ++= "\""
			out ++= pl.uuid.toString
			out ++= "\" -> \""
			out ++= typ.uuid.toString
			out ++= "\";\n"
		}
	}

	def print(typ: g.Type) {
		out ++= "\""
		out ++= typ.uuid.toString
		out ++= "\" [label=\""
		out ++= (typ match {
			case c: g.Type.Construct =>
				s"cons: ${c.name}"
		})
		out ++= "\"];\n"
		typ match {
			case cons: g.Type.Construct =>
				for((arg, i) <- cons.args.view.zipWithIndex) {
					out ++= "\""
					out ++= cons.uuid.toString
					out ++= "\" -> \""
					out ++= arg.uuid.toString
					out ++= "\" [label=\"arg: "
					out ++= i.toString
					out ++= "\"];\n"
				}

			case _ =>
		}
	}
}
