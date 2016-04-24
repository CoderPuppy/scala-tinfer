package cpup.typeInference

class DOTPrinter(val g: Graph, val prefix: String) {
	val out = new StringBuilder

	for(lbl <- g.typLabels)
		print(lbl)

	for(typ <- g.typs)
		print(typ)

	for(expr <- g.exprs)
		print(expr)

	for(use <- g.exprUses)
		print(use)

	for(lbl <- g.exprLabels)
		print(lbl)

	def print(lbl: g.Type.Label) {
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

	def name(expr: g.Expr) = expr match {
		case _: g.Expr.Unimplemented => "assume"
		case _: g.Expr.Call => "call"
		case _: g.Expr.Scope => "scope"
		case _: g.Expr.Function => "function"
		case _: g.Expr.Function#Arg => "arg"
		case _: g.Expr.Force => "force"
		case _: g.Expr.Isolate => "isolate"
	}

	def props(expr: g.Expr) = expr match {
		case _: g.Expr.Unimplemented =>
			",fillcolor=grey,fontcolor=red,style=filled"

		case _: g.Expr.Force =>
			",fillcolor=green,style=filled"

		case _: g.Expr.Call =>
			",fillcolor=red,style=filled,fontcolor=white"

		case _: g.Expr.Isolate =>
			",fillcolor=blue,style=filled,fontcolor=white"

		case _: g.Expr.Function =>
			",fillcolor=orange,style=filled,fontcolor=white"

		case _: g.Expr.Function#Arg =>
			",fillcolor=brown,style=filled,fontcolor=white"

		case _ => ""
	}

	def print(expr: g.Expr) {
		out ++= "\""
		out ++= prefix
		out ++= expr.uuid.toString
		out ++= "\" [label=\""
		out ++= name(expr)
		out ++= "\""
		out ++= props(expr)
		out ++= "];\n"

		for(use <- expr.uses) {
			out ++= "\""
			out ++= prefix
			out ++= expr.uuid.toString
			out ++= "\" -> \""
			out ++= prefix
			out ++= use.uuid.toString
			out ++= "\" [label=use];\n"
		}

		expr match {
			case u: g.Expr.Unimplemented =>
				out ++= "\""
				out ++= prefix
				out ++= expr.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.pl.typ.uuid.toString
				out ++= "\" [label=typ];\n"

			case f: g.Expr.Force =>
				out ++= "\""
				out ++= prefix
				out ++= expr.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= f.pl.typ.uuid.toString
				out ++= "\" [label=typ];\n"

				out ++= "\""
				out ++= prefix
				out ++= expr.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= f.wrapped.uuid.toString
				out ++= "\" [label=wrapped];\n"

			case i: g.Expr.Isolate =>
				for(expr <- i.expr) {
					out ++= "\""
					out ++= prefix
					out ++= i.uuid.toString
					out ++= "\" -> \""
					out ++= prefix
					out ++= expr.uuid.toString
					out ++= "\" [label=expr];\n"
				}

				for(exprUse <- i.exprUse) {
					out ++= "\""
					out ++= prefix
					out ++= i.uuid.toString
					out ++= "\" -> \""
					out ++= prefix
					out ++= exprUse.uuid.toString
					out ++= "\" [label=\"expr use\"];\n"
				}

				for(pat <- i.pattern) {
					out ++= "\""
					out ++= prefix
					out ++= i.uuid.toString
					out ++= "\" -> \""
					out ++= prefix
					out ++= pat.typ.uuid.toString
					out ++= "\" [label=pat];\n"
				}

			case c: g.Expr.Call =>
				out ++= "\""
				out ++= prefix
				out ++= c.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= c.fn.uuid.toString
				out ++= "\" [label=fn];\n"

				out ++= "\""
				out ++= prefix
				out ++= c.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= c.arg.uuid.toString
				out ++= "\" [label=arg];\n"

			case _ =>
		}
	}

	def print(use: g.Expr.Use) {
		out ++= "\""
		out ++= prefix
		out ++= use.uuid.toString
		out ++= "\" [label=\""
		out ++= name(use.expr)
		out ++= "\""
		out ++= props(use.expr)
		out ++= "];\n"

		out ++= "\""
		out ++= prefix
		out ++= use.uuid.toString
		out ++= "\" -> \""
		out ++= prefix
		out ++= use.expr.uuid.toString
		out ++= "\" [label=expr];\n"

		out ++= "\""
		out ++= prefix
		out ++= use.uuid.toString
		out ++= "\" -> \""
		out ++= prefix
		out ++= use.typ.typ.uuid.toString
		out ++= "\" [label=typ];\n"

		use match {
			case u: g.Expr.Function#Use =>
				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.Arg.uuid.toString
				out ++= "\" [label=arg];\n"

				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.Arg.use.uuid.toString
				out ++= "\" [label=\"arg use\"];\n"

				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.res.uuid.toString
				out ++= "\" [label=res];\n"

				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.resUse.uuid.toString
				out ++= "\" [label=\"res use\"];\n"

			case u: g.Expr.Call#Use =>
				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.fnUse.uuid.toString
				out ++= "\" [label=fn];\n"

				out ++= "\""
				out ++= prefix
				out ++= use.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.argUse.uuid.toString
				out ++= "\" [label=arg];\n"

			case u: g.Expr.Force#Use =>
				out ++= "\""
				out ++= prefix
				out ++= u.uuid.toString
				out ++= "\" -> \""
				out ++= prefix
				out ++= u.wrapped.uuid.toString
				out ++= "\" [label=wrapped];"

			case _ =>
		}
	}

	def print(lbl: g.Expr.Label) {
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
		out ++= lbl.expr.uuid.toString
		out ++= "\" [weight=200];\n"
	}
}
