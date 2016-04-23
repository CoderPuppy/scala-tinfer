package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		print("digraph {\n")
		print("nodesep=1;\n")

		val g = new Graph

		var step = 1
		def printGraph {
			System.gc()
			print("subgraph cluster_step" + step + " {\n")
			print("label = \"Step " + step + "\";\n")
			print(new DOTPrinter(g, s"step$step-").out.mkString)
			print("}\n")
			step += 1
		}

		val fn = g.typ("Function").label("fn")
		val io = g.typ("IO").label("io")
		val monad = g.typ("Monad").label("monad")
		val list = g.typ("List").label("list")
		val str = g.typ("String").label("str")
		val unit = g.typ("()").label("unit")
		val int = g.typ("Int").label("int")

		val one = g.typed(int).label("one")

		def fun(args: g.Place*)(res: g.Place): g.Place = {
			args.foldRight(res) { (arg, res) =>
				fn(arg)(res)
			}
		}

		// cons :: a -> [a] -> [a]
		val cons = g.scope { () =>
			val a = g.unknown
			g.typed(fun(a, list(a))(list(a))).label("cons").use
		}

		// plus :: Int -> Int -> Int
		val plus = g.typed(fun(int, int)(int)).label("plus")

		// inf :: Int -> [Int]
		// inf i = i : inf (i + 1)
		val inf = g.isolate
		inf.build(g.fn { i => cons(i)(inf(plus(i)(one))) })
		inf.label("inf")

		val infUse = inf.use
//		print(s"infUse -> ${'"'}step1-${infUse.uuid}${'"'};\n")

		println(infUse.typ.typ.asInstanceOf[g.Type.Construct].arg.uuid)
		println(inf.asInstanceOf[g.Expr.Isolate].pattern.get.typ.asInstanceOf[g.Type.Construct].arg.uuid)

//		printGraph

		print("}\n")
	}
}
