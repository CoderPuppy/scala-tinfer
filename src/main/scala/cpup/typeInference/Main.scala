package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		val graph = new Graph

		val string = graph.typ("String").label("string")
		val io = graph.typ("IO").label("io")
		val unit = graph.typ("()").label("unit")
		val fn = graph.typ("Function").label("fn")
		val int = graph.typ("Integer").label("int")

		// (>>=) :: m a -> (a -> m b) -> m b
		val m = graph.unknown.label("m")
		val a = graph.unknown.label("a")
		val b = graph.unknown.label("b")
		val bindArg1 = m(a).label("bindArg1")
		val bindArg2 = fn(a)(m(b)).label("bindArg2")
		val bindRes = m(b).label("bindRes")

		// getLine :: IO String
		val getLine = io(string).label("getLineRes")

		// putStrLn :: String -> IO ()
		val putStrLnArg = string.label("putStrLnArg")
		val putStrLnRes = int //io(unit).label("putStrLnRes")
		val putStrLn = fn(putStrLnArg)(putStrLnRes)

		// main = getLine >>= putStrLn
		val mainRes = graph.unknown.label("mainRes")

		bindArg1 ++ getLine
		bindArg2 ++ putStrLn
		mainRes ++ bindRes

		println("digraph {\n")
		System.gc()
		println(new DOTPrinter(graph).out.mkString)
		println("}")
	}
}
