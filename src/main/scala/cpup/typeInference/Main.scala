package cpup.typeInference

object Main {
	def main(args: Array[String]) {
		val graph = new Graph
		// call :: (a -> b) -> a -> b
		// call f a = f a
		val arg = graph.unknown.label("arg")
		val res = graph.unknown.label("res")
		val fn = graph.typ("Func", arg, res).label("fn")

		// id :: a -> a
		// main = call test 1
		val a = graph.unknown.label("a")
		val id = graph.typ("Func", a, a).label("id")

		fn ++ id
		arg ++ graph.typ("Int")

		println("digraph {\n")
		System.gc()
		println(new DOTPrinter(graph).out.mkString)
		println("}")
	}
}
