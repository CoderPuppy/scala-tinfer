package cpup.tinfer

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

		val fn = g.typ("Function")
		val io = g.typ("IO")
		val monad = g.typ("Monad")
		val list = g.typ("List")
		val str = g.typ("String")
		val unit = g.typ("()")
		val int = g.typ("Int")

		val one = g.uimpl.force(int).label("one")
		val zero = g.uimpl.force(int).label("zero")

		def fun(args: g.Place*)(res: g.Place): g.Place = {
			args.foldRight(res) { (arg, res) =>
				fn(arg)(res)
			}
		}

		// Monad-IO :: Monad IO
		val mndIO = g.isolate.build(
			g.uimpl.force(monad(io))
		).label("mndIO")

		// Monad-List :: Monad List
		val mndList = g.isolate.build(
			g.uimpl.force(monad(list))
		).label("mndList")

		// bind :: Monad m -> m a -> (a -> m b) -> m b
		val bind = g.isolate.build({
			val m = g.unknown.label("m")
			val a = g.unknown.label("a")
			val b = g.unknown.label("b")
			g.uimpl.force(fun(monad(m), m(a), fun(a)(m(b)))(m(b)))
		}).label("bind")

		// seq :: Monad m -> m a -> m b -> m b
		// seq m a b = bind m a (\_ -> b)
		val seq = g.isolate.build(
			g.fn { m => g.fn { a => g.fn { b =>
				bind(m)(a)(g.fn { _ => b })
			} } }
		).label("seq")

		// getLine :: IO String
		val getLine = g.isolate.build(
			g.uimpl.force(io(str))
		).label("getLine")

		// putStrLn :: String -> IO ()
		val putStrLn = g.isolate.build(
			g.uimpl.force(fun(str)(io(unit)))
		).label("putStrLn")

		// main = seq Monad (bind Monad getLine putStrLn) getLine
		val main = g.isolate.build(
			seq(mndIO)(bind(mndIO)(getLine)(putStrLn))(getLine)
		).label("main")

		// repeat :: a -> Int -> [a]
		// repeat _ 0 = []
		// repeat a n = a : repeat a (n - 1)
		val repeat = g.isolate.build({
			val a = g.unknown.label("a")
			g.uimpl.force(fun(a, int)(list(a)))
		}).label("repeat")

		// cons :: a -> [a] -> [a]
		val cons = g.isolate.build({
			val a = g.unknown.label("a")
			g.uimpl.force(fun(a, list(a))(list(a)))
		}).label("cons")

		// plus :: Int -> Int -> Int
		val plus = g.isolate.build(
			g.uimpl.force(fun(int, int)(int))
		).label("plus")

		// inf :: Int -> [Int]
		// inf i = i : inf (i + 1)
		val inf = g.isolate
		inf.build(g.fn { i => cons(i)(inf(plus(i)(one))) })
		inf.label("inf")

		// test :: a -> [a]
		// test a = bind Monad inf (repeat a)
		val test = g.isolate.build(
			g.fn { a =>
				bind(mndList)(inf(zero))(repeat(a))
			}
		).label("test")

		printGraph

		print("}\n")
	}
}
