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

		val one = g.undefined.force(int).label("one")

		def fun(args: g.Place*)(res: g.Place): g.Place = {
			args.foldRight(res) { (arg, res) =>
				fn(arg)(res)
			}
		}

		// Monad :: Monad IO
		val mnd = g.scope { () =>
			g.undefined.force(monad(io || list)).label("mnd").use
    }

		// bind :: Monad m -> m a -> (a -> m b) -> m b
		val bind = g.scope { () =>
			val m = g.unknown.label("bind m")
			val a = g.unknown.label("bind a")
			val b = g.unknown.label("bind b")

			g.undefined.force(fun(monad(m), m(a), fun(a)(m(b)))(m(b))).label("bind").use
		}

		// seq :: Monad m -> m a -> m b -> m b
		val seq = g.fn { m => g.fn { a => g.fn { b => bind(m)(a)(g.fn { _ => b }) } } }.label("seq")

		// getLine :: IO String
		val getLine = g.scope { () =>
			g.undefined.force(io(str)).label("getLine").use
		}

		// putStrLn :: String -> IO ()
		val putStrLn = g.scope { () =>
			g.undefined.force(fun(str)(io(unit))).label("putStrLn").use
		}

		// main = bind getLine putStrLn
		val main = seq(mnd)(bind(mnd)(getLine)(putStrLn))(getLine).label("main")

		// repeat :: a -> Int -> [a]
		val repeat = g.scope { () =>
			val a = g.unknown.label("repeat a")
			g.undefined.force(fun(a, int)(list(a))).label("repeat").use
		}

		// inf :: [Int]
		val inf = g.undefined.force(list(int)).label("inf")

		// test :: a -> [a]
		val test = g.fn { a => bind(mnd)(inf)(repeat(a)) }.label("test")

		// test1 :: [Int]
		// test1 = test 1
		val test1 = test.label("test1 test")(one).label("test1")

		// testMnd :: [Monad List]
		// testMnd = test Monad
		val testMnd = test.label("testMnd test")(mnd).label("testMnd").force(list(monad(list)))

		val mainUse = main.use
		val testUse = test.use
		testUse.typ.label("Main - test")
		val test1Use = test1.use
		test1Use.typ.label("Main - test1")
		val testMndUse = testMnd.use
		testMndUse.typ.label("Main - testMnd")

		val mndUse = mnd.use
		mndUse.typ.label("Main - mnd")

		printGraph

		print("}\n")
	}
}
