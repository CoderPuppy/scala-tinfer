package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		println("digraph {\n")

		val graph = new Graph

		var step = 1
		def printGraph {
			System.gc()
			println("subgraph cluster_step" + step + " {\n")
			println("label = \"Step " + step + "\";\n")
			println(new DOTPrinter(graph, s"step$step-").out.mkString)
			println("}\n")
			step += 1
		}

		val string = graph.typ("String").label("string")
		val io = graph.typ("IO").label("io")
		val unit = graph.typ("()").label("unit")
		val fn = graph.typ("Function").label("fn")
		val int = graph.typ("Integer").label("int")

		// (>>=) :: m a -> (a -> m b) -> m b
		val m = graph.unknown.label("m")
		val a = graph.unknown.label("a")
		val b = graph.unknown.label("b")
		val bind = fn(m(a))(fn(fn(a)(m(b)))(m(b))).label("bind")
		val bindArg1 = m(a).label("bindArg1")
		val bindArg2 = fn(a)(m(b)).label("bindArg2")
		val bindRes = m(b).label("bindRes")

		// getLine :: IO String
		val getLine = io(string).label("getLineRes")

		// putStrLn :: String -> IO ()
		val putStrLnArg = string.label("putStrLnArg")
		val putStrLnRes = io(unit).label("putStrLnRes")
		val putStrLn = fn(putStrLnArg)(putStrLnRes)

		// main = getLine >>= putStrLn
		val mainRes = graph.unknown.label("mainRes")

		printGraph

		bindArg1 ++ getLine
		printGraph

		bindArg2 ++ putStrLn
		printGraph

		mainRes ++ bindRes
		printGraph

		println("}")
	}
}
