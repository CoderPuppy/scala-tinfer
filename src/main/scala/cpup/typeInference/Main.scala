package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		print("digraph {\n")

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
		val str = g.typ("String").label("str")
		val unit = g.typ("()").label("unit")

		def fun(args: g.Place*)(res: g.Place): g.Place = {
			args.foldRight(res) { (arg, res) =>
				fn(arg)(res)
			}
		}

		// io-monad :: Monad IO
		val ioM = g.undefined.force(monad(io)).label("ioM")

		// bind :: Monad m -> m a -> (a -> m b) -> m b
		val bind = g.scope { () =>
			val m = g.unknown.label("m")
			val a = g.unknown.label("a")
			val b = g.unknown.label("b")

			g.undefined.force(fun(monad(m), m(a), fun(a)(m(b)))(m(b))).label("bind").use
		}

		// seq :: Monad m -> m a -> m b -> m b
		val seq = g.fn { m => g.fn { a => g.fn { b => bind(m)(a)(g.fn { _ => b }) } } }.label("seq")

		// getLine :: IO String
		val getLine = g.scope { () =>
			g.undefined.force(io(str)).use
		}

		// putStrLn :: String -> IO ()
		val putStrLn = g.scope { () =>
			g.undefined.force(fun(str)(io(unit))).use
		}

		// main = bind getLine putStrLn
		val main = seq(ioM)(bind(ioM)(getLine)(putStrLn))(getLine)

		main.use.typ.label("main")

		printGraph

		print("}\n")
	}
}
