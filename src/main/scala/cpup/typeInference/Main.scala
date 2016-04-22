package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		println("digraph {\n")

		val g = new Graph

		var step = 1
		def printGraph {
			System.gc()
			println("subgraph cluster_step" + step + " {\n")
			println("label = \"Step " + step + "\";\n")
			println(new DOTPrinter(g, s"step$step-").out.mkString)
			println("}\n")
			step += 1
		}

		val fn = g.typ("Function").label("fn")
		val io = g.typ("IO").label("io")
		val str = g.typ("String").label("str")
		val unit = g.typ("()").label("unit")

		// bind :: m a -> (a -> m b) -> m b
		val bind = g.scope { () =>
			val m = g.unknown.label("m")
			val a = g.unknown.label("a")
			val b = g.unknown.label("b")

			g.undefined.use.force(fn(m(a))(fn(fn(a)(m(b)))(m(b))).label("bind"))
		}

		// seq :: m a -> m b -> m b
		val seq = g.fn { a => g.fn { b => bind(a)(g.fn { _ => b }) } }.label("seq")

		// getLine :: IO String
		val getLine = g.scope { () =>
			g.undefined.use.force(io(str))
		}

		// putStrLn :: String -> IO ()
		val putStrLn = g.scope { () =>
			g.undefined.use.force(fn(str)(io(unit)))
		}

		// main = bind getLine putStrLn
		val main = seq(bind(getLine)(putStrLn))(getLine)

		main.use.typ.label("main")

		printGraph

		println("}")
	}
}
