package impl.ast2

import impl.logic._

object Compiler {

  def compile(ast: Ast.Root): Ast.Root = {
    val ast2 = Desugaring.desugar(ast)
    val env = Symbols.visit(ast)
    val ast3 = Disambiguation.disambiguate(ast, env)
    //println(env)

    ast3
  }

  /**
   * A compiler-phases which constructs environments (i.e. the symbol table).
   */
  object Symbols {

    /**
     * A (fully qualified) name is a list of strings.
     */
    // TODO Move into Ast and change to Seq.
    type Name = List[String]

    /**
     * An environment is map from names to ast declaractions.
     *
     * An environment may contain multiple declaractions for the same names:
     *
     * (1) Names may be overloaded for values, types, etc.
     * (2) Names may be ambiguous.
     */
    // Todo: Move into some abstract compiler trait.
    type Environment = MultiMap[Name, Ast.Declaration]

    /**
     * The empty environment.
     */
    val Empty = MultiMap.empty[Name, Ast.Declaration]

    /**
     * Returns an environment with the given mapping.
     */
    def environmentOf(kv: (Name, Ast.Declaration)): Environment = MultiMap(kv)

    /**
     * Returns a map from fully qualified names to ast declarations.
     */
    def visit(ast: Ast.Root): Environment = ast match {
      case Ast.Root(decls) => (decls foldLeft Empty) {
        case (env, decl) => env ++ visit(Nil, decl)
      }
    }

    /**
     * Returns a map from fully qualified names to ast declaractions assuming the declarations reside under the given namespace.
     */
    def visit(namespace: Name, ast: Ast.Declaration): Environment = ast match {
      case Ast.Declaration.NameSpace(name, body) => (body foldLeft Empty) {
        case (env, decl) => env ++ visit(withSuffix(namespace, name), decl)
      }
      case decl: Ast.Declaration.Tpe => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Val => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Var => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Fun => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Enum =>
        val init = environmentOf(withSuffix(namespace, decl.name) -> decl)
        decl.tpe.elms.foldLeft(init) {
          case (env, tag) => env ++ environmentOf(withSuffix(namespace, tag.name) -> decl)
        }
      case decl: Ast.Declaration.Lattice => Empty
      case decl: Ast.Declaration.Fact => Empty
      case decl: Ast.Declaration.Rule => Empty
    }

    /**
     * Returns `name` . `suffix`.
     */
    def withSuffix(name: Name, suffix: String): Name = name ::: List(suffix)

    /**
     * Returns `name` . `suffix`.
     */
    def withSuffix(name: Name, suffix: Seq[String]): Name = name ::: suffix.toList
  }


  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.
  /**
   * A compiler-phase which replaces name references by their actuals.
   */
  object Disambiguation {

    import Symbols._

    /**
     * Disambiguates the given `ast` using the given environment `env`.
     */
    def disambiguate(ast: Ast.Root, env: Environment): Ast.Root = Ast.Root(ast.decls map {
      case decl => disambiguate(Nil, decl, env)
    })

    /**
     * Disambiguates the given `ast` under the current `namespace` using the given `environment`.
     */
    def disambiguate(namespace: Name, ast: Ast.Declaration, env: Environment): Ast.Declaration = ast match {
      case Ast.Declaration.NameSpace(name, body) => Ast.Declaration.NameSpace(name, body map {
        case decl => disambiguate(withSuffix(namespace, name), decl, env)
      })
      case decl: Ast.Declaration.Lattice => decl.copy(record = disambiguate(namespace, decl.record, env, Set.empty))
      case _ => ast
    }

    /**
     * Replaces all ambiguous names in the given expression.
     */
    def disambiguate(namespace: Name, ast: Ast.Expression, env: Environment, bound: Set[String]): Ast.Expression = ast match {
      case Ast.Expression.AmbiguousName(name) => lookupExp(namespace, name.toList, env)
      case e: Ast.Expression.Lit => e
      case Ast.Expression.Unary(op, e) => Ast.Expression.Unary(op, disambiguate(namespace, e, env, bound))
      case Ast.Expression.Binary(e1, op, e2) => Ast.Expression.Binary(disambiguate(namespace, e1, env, bound), op, disambiguate(namespace, e2, env, bound))
      case Ast.Expression.Record(elms) => Ast.Expression.Record(elms map {
        case (name, e) => (name, disambiguate(namespace, e, env, bound))
      })
    }


    // TODO: Messy. Rewrite.
    def lookupExp(namespace: Name, name: Name, env: Environment): Ast.Expression = {
      lookupExp(namespace ::: name, env).
        orElse(lookupExp(name, env)).getOrElse(throw new CompilerException(s"Name not found $name"))
    }

    def lookupExp(name: Name, env: Environment): Option[Ast.Expression] = {
      val candidates = env.get(name).collect {
        case d: Ast.Declaration.Val => d.exp
        case d: Ast.Declaration.Fun => d.body
        case d: Ast.Declaration.Enum => d.tpe.elms.find(tag => tag.name == name.last).map(tag => Ast.Expression.Tag(tag.name, Ast.Expression.Unit)).get
      }
      if (candidates.size > 1) {
        throw new CompilerException(s"Ambiguous name: $name")
      }
      candidates.headOption
    }

  }


  case class CompilerException(msg: String) extends RuntimeException(msg)


  // TODO: Move somewhere appropiate.
  object MultiMap {
    def empty[K, V]: MultiMap[K, V] = new MultiMap[K, V](Map.empty[K, Set[V]])

    def apply[K, V](kv: (K, V)): MultiMap[K, V] = new MultiMap[K, V](Map[K, Set[V]](kv._1 -> Set(kv._2)))
  }

  class MultiMap[K, V](val m: Map[K, Set[V]]) {
    def get(k: K): Set[V] = m.getOrElse(k, Set.empty[V])

    def ++(that: MultiMap[K, V]): MultiMap[K, V] = new MultiMap(
      (that.m foldLeft this.m) {
        case (acc, (thatKey, thatValues)) =>
          val thisValues = acc.getOrElse(thatKey, Set.empty)
          acc + (thatKey -> (thisValues ++ thatValues))
      }
    )

    override def toString: String = m.toString()
  }

}
