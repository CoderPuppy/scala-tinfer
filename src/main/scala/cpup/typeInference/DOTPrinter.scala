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
		out ++= "\",fillcolor=yellow,style=filled];\n"

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

			case o: g.Type.Or =>
				"or"
		})
		out ++= "\""
		typ match {
			case _: g.Type.Unknown =>
				out ++= ",fillcolor=grey,fontcolor=red,style=filled"

			case _: g.Type.Identifier =>
				out ++= ",fillcolor=green,style=filled"

			case _: g.Type.Construct =>
				out ++= ",fillcolor=red,style=filled,fontcolor=white"

			case _: g.Type.Or =>
				out ++= ",fillcolor=blue,style=filled,fontcolor=white"

			case _ =>
		}
		out ++= "];\n"
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

			case or: g.Type.Or =>
				for(opt <- or.optsT) {
					out ++= "\""
					out ++= prefix
					out ++= or.uuid.toString
					out ++= "\" -> \""
					out ++= prefix
					out ++= opt.uuid.toString
					out ++= "\" [label=opt];\n"
				}

			case _ =>
		}
	}
}
