package cpup.typeInference

import java.util.regex.Pattern

class DOTPrinter(g: Graph) {
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
				s"unlabeled: ${pl.name}"
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

			case u: g.Type.Unknown =>
				s"unknown: ${u.name}"
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
