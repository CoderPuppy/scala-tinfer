package cpup.typeInference

import java.util.regex.Pattern

class DOTPrinter(g: Graph) {
	private var unknownNames = Stream.from(1).flatMap { n =>
		('A' to 'Z').map { c =>
			c.toString * n
		}
	}
	private var unlabeledNames = Stream.from(1).flatMap { n =>
		('A' to 'Z').map { c =>
			c.toString * n
		}
	}

	val out = new StringBuilder

	for(pl <- g.places) {
		print(pl)
	}

	for(typ <- g.typs)
		print(typ)

	def print(pl: g.Place) {
		val label = pl.label match {
			case Some(label) =>
				s"label: $label"

			case _ =>
				val label = s"unlabeled: ${unlabeledNames.head}"
				unlabeledNames = unlabeledNames.tail
				label
		}
		out ++= "\""
		out ++= pl.uuid.toString
		out ++= "\" [label=\""
		out ++= label.replace("\\", "\\\\").replace("\"", "\\\"")
		out ++= "\"]\n"
		out ++= "\""
		out ++= pl.uuid.toString
		out ++= "\" -> \""
		out ++= pl.typ.uuid.toString
		out ++= "\"\n"
	}

	def print(typ: g.Type) {
		out ++= "\""
		out ++= typ.uuid.toString
		out ++= "\" [label=\""
		out ++= (typ match {
			case c: g.Type.Construct =>
				"cons"

			case i: g.Type.Identifier =>
				s"ident: ${i.name}"

			case _: g.Type.Unknown =>
				val name = unknownNames.head
				unknownNames = unknownNames.tail
				s"unknown: $name"
		})
		out ++= "\"];\n"
		typ match {
			case cons: g.Type.Construct =>
				out ++= "\""
				out ++= cons.uuid.toString
				out ++= "\" -> \""
				out ++= cons.fn.uuid.toString
				out ++= "\" [label=\"fn\"];\n"

				out ++= "\""
				out ++= cons.uuid.toString
				out ++= "\" -> \""
				out ++= cons.arg.uuid.toString
				out ++= "\" [label=\"arg\"];\n"

			case _ =>
		}
	}
}
