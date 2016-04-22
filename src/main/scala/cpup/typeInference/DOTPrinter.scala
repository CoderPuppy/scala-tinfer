package cpup.typeInference

class DOTPrinter(val g: Graph, val prefix: String) {
	val out = new StringBuilder

	for(lbl <- g.labels)
		print(lbl)

	for(typ <- g.typs)
		print(typ)

	def print(lbl: g.Label): Unit = {
		out ++= "\""
		out ++= prefix
		out ++= lbl.uuid.toString
		out ++= "\" [label=\"label: "
		out ++= lbl.label.replace("\\", "\\\\").replace("\"", "\\\"")
		out ++= "\"];\n"

		out ++= "\""
		out ++= prefix
		out ++= lbl.uuid.toString
		out ++= "\" -> \""
		out ++= prefix
		out ++= lbl.place.typ.uuid.toString
		out ++= "\";\n"
	}

	def print(typ: g.Type) {
		out ++= "\""
		out ++= prefix
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
				out ++= prefix
				out ++= cons.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= cons.fn.typ.uuid.toString
				out ++= "\" [label=\"fn\"];\n"

				out ++= "\""
				out ++= prefix
				out ++= cons.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= cons.arg.typ.uuid.toString
				out ++= "\" [label=\"arg\"];\n"

			case _ =>
		}
	}
}
