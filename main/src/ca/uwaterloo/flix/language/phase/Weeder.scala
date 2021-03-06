/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import java.math.BigInteger

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder {

  /**
    * Weeds the whole program.
    */
  def weed(program: ParsedAst.Program, hooks: Map[Symbol.Resolved, Ast.Hook]): Validation[WeededAst.Program, WeederError] = {
    val b = System.nanoTime()
    @@(program.roots map weed) map {
      case roots =>
        val e = System.nanoTime() - b
        WeededAst.Program(roots, hooks, program.time.copy(weeder = e))
    }
  }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def weed(root: ParsedAst.Root): Validation[WeededAst.Root, WeederError] = {
    @@(root.decls map Declarations.weed) map {
      case decls => WeededAst.Root(decls)
    }
  }

  object Declarations {

    /**
      * Compiles the given parsed declaration `past` to a weeded declaration.
      */
    def weed(decl: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = decl match {
      case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Namespace(name, ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Definition(ann, sp1, ident, paramsOpt, tpe, exp, sp2) =>
        val sl = mkSL(ident.sp1, ident.sp2)
        val annVal = Annotations.weed(ann)
        val expVal = Expressions.weed(exp)

        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => @@(annVal, expVal) flatMap {
            case (as, e) => WeededAst.Declaration.Definition(as, ident, Nil, e, Type.Lambda(Nil, tpe), sl).toSuccess
          }
          case Some(Nil) => IllegalParameterList(sl).toFailure
          case Some(params) =>
            /*
             * Check for `DuplicateFormal`.
             */
            val formalsVal = checkDuplicateFormal(params)
            @@(annVal, formalsVal, expVal) map {
              case (as, fs, e) =>
                val t = Type.Lambda(fs map (_.tpe), tpe)
                WeededAst.Declaration.Definition(as, ident, fs, e, t, sl)
            }
        }

      case ParsedAst.Declaration.Signature(sp1, ident, paramsOpt, tpe, sp2) =>
        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => WeededAst.Declaration.Signature(ident, Nil, tpe, mkSL(sp1, sp2)).toSuccess
          case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
          case Some(params) => WeededAst.Declaration.Signature(ident, params, tpe, mkSL(sp1, sp2)).toSuccess
        }

      case ParsedAst.Declaration.External(sp1, ident, paramsOpt, tpe, sp2) =>
        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => WeededAst.Declaration.External(ident, Nil, tpe, mkSL(sp1, sp2)).toSuccess
          case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
          case Some(params) => WeededAst.Declaration.External(ident, params, tpe, mkSL(sp1, sp2)).toSuccess
        }

      case ParsedAst.Declaration.Law(sp1, ident, tparams, paramsOpt, tpe, exp, sp2) =>
        /*
         * Check for `IllegalParameterList`.
         */
        Expressions.weed(exp) flatMap {
          case e => paramsOpt match {
            case None => WeededAst.Declaration.Law(ident, tparams, Nil, tpe, e, mkSL(sp1, sp2)).toSuccess
            case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
            case Some(params) => WeededAst.Declaration.Law(ident, tparams, params, tpe, e, mkSL(sp1, sp2)).toSuccess
          }
        }

      case ParsedAst.Declaration.Enum(sp1, ident, cases, sp2) =>
        /*
         * Check for `DuplicateTag`.
         */
        Validation.fold[ParsedAst.Case, Map[String, WeededAst.Case], WeederError](cases, Map.empty) {
          case (macc, caze: ParsedAst.Case) =>
            val tag = caze.ident.name
            macc.get(tag) match {
              case None => (macc + (tag -> WeededAst.Case(ident, caze.ident, caze.tpe))).toSuccess
              case Some(otherTag) => DuplicateTag(tag, otherTag.tag.loc, mkSL(caze.sp1, caze.sp2)).toFailure
            }
        } map {
          case m => WeededAst.Declaration.Enum(ident, m, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Class(sp1, ident, tparams, bounds, decls, sp2) =>
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Class(ident, tparams, ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Impl(sp1, ident, tparams, bounds, decls, sp2) =>
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Impl(ident, tparams, ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Relation(sp1, ident, attrs, sp2) =>
        /*
         * Check for `EmptyRelation`
         */
        if (attrs.isEmpty)
          return EmptyRelation(mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as => WeededAst.Table.Relation(ident, as, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Lattice(sp1, ident, attrs, sp2) =>
        /*
         * Check for `EmptyLattice`.
         */
        if (attrs.isEmpty)
          return EmptyLattice(mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as =>
            // Split the attributes into keys and element.
            WeededAst.Table.Lattice(ident, as.init, as.last, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Fact(sp1, head, sp2) =>
        Predicate.Head.weed(head) map {
          case p => WeededAst.Declaration.Fact(p, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Rule(sp1, head, body, sp2) =>
        // compute an map from variable names to alias predicates.
        val as = body collect {
          case p: ParsedAst.Predicate.Equal => p
        }
        val aliasesVal = Validation.fold[ParsedAst.Predicate.Equal, Map[String, ParsedAst.Predicate.Equal], WeederError](as, Map.empty) {
          case (m, p) => m.get(p.ident.name) match {
            case None => (m + (p.ident.name -> p)).toSuccess
            case Some(otherAlias) => DuplicateAlias(p.ident.name, mkSL(otherAlias.sp1, otherAlias.sp2), mkSL(p.sp1, p.sp2)).toFailure
          }
        }

        aliasesVal flatMap {
          case aliases =>
            val headVal = Predicate.Head.weed(head, aliases)
            val bodyVal = @@(body.filterNot(_.isInstanceOf[ParsedAst.Predicate.Equal]).map(Predicate.Body.weed))

            @@(headVal, bodyVal) map {
              case (h, b) => WeededAst.Declaration.Rule(h, b, mkSL(sp1, sp2))
            }
        }

      case ParsedAst.Declaration.Index(sp1, ident, indexes, sp2) =>
        /*
         * Check for `EmptyIndex` and `IllegalIndex`.
         */
        val sl = mkSL(sp1, sp2)
        if (indexes.isEmpty)
          EmptyIndex(sl).toFailure
        else if (indexes.exists(_.isEmpty))
          IllegalIndex(sl).toFailure
        else
          WeededAst.Declaration.Index(ident, indexes, sl).toSuccess

      case ParsedAst.Declaration.BoundedLattice(sp1, tpe, elms, sp2) =>
        val elmsVal = @@(elms.toList.map(Expressions.weed))
        elmsVal flatMap {
          case List(bot, top, leq, lub, glb) => WeededAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, mkSL(sp1, sp2)).toSuccess
          case _ => IllegalLattice(mkSL(sp1, sp2)).toFailure
        }

    }

  }

  object Literals {
    // TODO: Remove once terms have been unified with expressions.
    def compile(literal: ParsedAst.Literal): Validation[WeededAst.Literal, WeederError] = literal match {
      case ParsedAst.Literal.Unit(sp1, sp2) =>
        WeededAst.Literal.Unit(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.True(sp1, sp2) =>
        WeededAst.Literal.True(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.False(sp1, sp2) =>
        WeededAst.Literal.False(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Char(sp1, lit, sp2) =>
        WeededAst.Literal.Char(lit(0), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Float32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Float64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int8(sp1, sign, digits, sp2) =>
        toInt8(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Int8(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int16(sp1, sign, digits, sp2) =>
        toInt16(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Int16(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) =>
        toInt32(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Int32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int64(sp1, sign, digits, sp2) =>
        toInt64(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.Int64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.BigInt(sp1, sign, digits, sp2) =>
        toBigInt(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Literal.BigInt(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Literal.Str(lit, mkSL(sp1, sp2)).toSuccess
    }
  }


  object Expressions {

    /**
      * Translates the given literal to an expression.
      */
    def toExp(lit0: ParsedAst.Literal): Validation[WeededAst.Expression, WeederError] = lit0 match {
      case ParsedAst.Literal.Unit(sp1, sp2) =>
        WeededAst.Expression.Unit(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.True(sp1, sp2) =>
        WeededAst.Expression.True(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.False(sp1, sp2) =>
        WeededAst.Expression.False(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Char(sp1, lit, sp2) =>
        WeededAst.Expression.Char(lit(0), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Float32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Float64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int8(sp1, sign, digits, sp2) =>
        toInt8(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int8(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int16(sp1, sign, digits, sp2) =>
        toInt16(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int16(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) =>
        toInt32(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int64(sp1, sign, digits, sp2) =>
        toInt64(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.BigInt(sp1, sign, digits, sp2) =>
        toBigInt(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.BigInt(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Expression.Str(lit, mkSL(sp1, sp2)).toSuccess
    }

    /**
      * Weeds the given expression.
      */
    def weed(exp0: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = exp0 match {
      case ParsedAst.Expression.Wild(sp1, sp2) =>
        IllegalWildcard(mkSL(sp1, sp2)).toFailure

      case ParsedAst.Expression.Var(sp1, name, sp2) =>
        WeededAst.Expression.Var(name, mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Lit(sp1, lit, sp2) => toExp(lit)

      case ParsedAst.Expression.Apply(lambda, args, sp2) =>
        val sp1 = leftMostSourcePosition(lambda)
        @@(weed(lambda), @@(args map weed)) flatMap {
          case (e, as) => WeededAst.Expression.Apply(e, as, mkSL(sp1, sp2)).toSuccess
        }

      case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
        /*
         * Rewrites infix expressions to apply expressions.
         */
        @@(weed(exp1), weed(exp2)) map {
          case (e1, e2) =>
            val loc = mkSL(leftMostSourcePosition(exp1), sp2)
            val e3 = WeededAst.Expression.Var(name, loc)
            WeededAst.Expression.Apply(e3, List(e1, e2), loc)
        }

      case ParsedAst.Expression.Lambda(sp1, params, exp, sp2) =>
        weed(exp) map {
          case e => WeededAst.Expression.Lambda(params.toList, e, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Unary(sp1, op, exp, sp2) => weed(exp) map {
        case e => WeededAst.Expression.Unary(op, e, mkSL(sp1, sp2))
      }

      case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
        @@(weed(exp1), weed(exp2)) map {
          case (e1, e2) => WeededAst.Expression.Binary(op, e1, e2, mkSL(leftMostSourcePosition(exp1), sp2))
        }

      case ParsedAst.Expression.ExtendedBinary(exp1, op, exp2, sp2) =>
        /*
         * Rewrites extended binary expressions to apply expressions.
         */
        @@(weed(exp1), weed(exp2)) map {
          case (e1, e2) =>
            op match {
              case ExtBinaryOperator.Leq =>
                val sp1 = leftMostSourcePosition(exp1)
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊑", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Lub =>
                val sp1 = leftMostSourcePosition(exp1)
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊔", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Glb =>
                val sp1 = leftMostSourcePosition(exp1)
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "⊓", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Widen =>
                val sp1 = leftMostSourcePosition(exp1)
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "▽", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

              case ExtBinaryOperator.Narrow =>
                val sp1 = leftMostSourcePosition(exp1)
                val loc = mkSL(sp1, sp2)
                val ident = Name.Ident(sp1, "△", sp2)
                val namespace = Name.NName(sp1, List.empty, sp2)
                val name = Name.QName(sp1, namespace, ident, sp2)
                val lambda = WeededAst.Expression.Var(name, loc)
                WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
            }
        }

      case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
        @@(weed(exp1), weed(exp2), weed(exp3)) map {
          case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.LetMatch(sp1, pat, exp1, exp2, sp2) =>
        /*
         * Rewrites a let-match to a regular let-binding or a full-blown pattern match.
         */
        @@(Patterns.weed(pat), weed(exp1), weed(exp2)) map {
          case (WeededAst.Pattern.Var(ident, loc), value, body) =>
            // Let-binding
            WeededAst.Expression.Let(ident, value, body, mkSL(sp1, sp2))
          case (pattern, value, body) =>
            // Full-blown pattern match.
            val rules = List(pattern -> body)
            WeededAst.Expression.Match(value, rules, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
        val rulesVal = rules map {
          case (pat, body) => @@(Patterns.weed(pat), weed(body))
        }
        @@(weed(exp), @@(rulesVal)) map {
          case (e, rs) => WeededAst.Expression.Match(e, rs, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Switch(sp1, rules, sp2) =>
        val rulesVal = rules map {
          case (cond, body) => @@(Expressions.weed(cond), Expressions.weed(body))
        }
        @@(rulesVal) map {
          case rs => WeededAst.Expression.Switch(rs, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Tag(sp1, enum, tag, o, sp2) =>
        /*
         * Introduce implicit unit, if needed.
         */
        o match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val exp = WeededAst.Expression.Unit(loc)
            WeededAst.Expression.Tag(enum, tag, exp, loc).toSuccess
          case Some(exp) => weed(exp) map {
            case e => WeededAst.Expression.Tag(enum, tag, e, mkSL(sp1, sp2))
          }
        }

      case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
        /*
         * Rewrites empty tuples to Unit and eliminate single-element tuples.
         */
        @@(elms map weed) map {
          case Nil =>
            val loc = mkSL(sp1, sp2)
            WeededAst.Expression.Unit(loc)
          case x :: Nil => x
          case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FNone(sp1, sp2) =>
        WeededAst.Expression.FNone(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.FSome(sp1, exp, sp2) =>
        weed(exp) map {
          case e => WeededAst.Expression.FSome(e, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FNil(sp1, sp2) =>
        WeededAst.Expression.FNil(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.FList(hd, tl, sp2) =>
        val sp1 = leftMostSourcePosition(hd)
        @@(weed(hd), weed(tl)) map {
          case (e1, e2) => WeededAst.Expression.FList(e1, e2, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FVec(sp1, elms, sp2) =>
        @@(elms map weed) map {
          case es => WeededAst.Expression.FVec(es, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
        @@(elms map weed) map {
          case es => WeededAst.Expression.FSet(es, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.FMap(sp1, elms, sp2) =>
        val elmsVal = elms map {
          case (key, value) => @@(weed(key), weed(value))
        }

        @@(elmsVal) map {
          case es => WeededAst.Expression.FMap(es, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.GetIndex(sp1, exp1, exp2, sp2) =>
        @@(weed(exp1), weed(exp2)) map {
          case (e1, e2) => WeededAst.Expression.GetIndex(e1, e2, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.PutIndex(sp1, exp1, exp2, exp3, sp2) =>
        @@(weed(exp1), weed(exp2), weed(exp3)) map {
          case (e1, e2, e3) => WeededAst.Expression.PutIndex(e1, e2, e3, mkSL(sp1, sp2))
        }

      case ParsedAst.Expression.Existential(sp1, paramsOpt, exp, sp2) =>
        /*
         * Checks for `IllegalExistential`.
         */
        weed(exp) flatMap {
          case e => paramsOpt match {
            case None => IllegalExistential("An existential quantifier must have at least one parameter.", mkSL(sp1, sp2)).toFailure
            case Some(Nil) => IllegalExistential("An existential quantifier must have at least one parameter.", mkSL(sp1, sp2)).toFailure
            case Some(params) =>
              /*
               * Check for `DuplicateFormal`.
               */
              checkDuplicateFormal(params) map {
                case ps => WeededAst.Expression.Existential(ps, e, mkSL(sp1, sp2))
              }
          }
        }

      case ParsedAst.Expression.Universal(sp1, paramsOpt, exp, sp2) =>
        /*
         * Checks for `IllegalExistential`.
         */
        weed(exp) flatMap {
          case e => paramsOpt match {
            case None => IllegalUniversal("A universal quantifier must have at least one parameter.", mkSL(sp1, sp2)).toFailure
            case Some(Nil) => IllegalUniversal("An universal quantifier must have at least one parameter.", mkSL(sp1, sp2)).toFailure
            case Some(params) =>
              /*
               * Check for `DuplicateFormal`.
               */
              checkDuplicateFormal(params) map {
                case ps => WeededAst.Expression.Universal(ps, e, mkSL(sp1, sp2))
              }
          }
        }

      case ParsedAst.Expression.Ascribe(exp, tpe, sp2) =>
        weed(exp) map {
          case e => WeededAst.Expression.Ascribe(e, tpe, mkSL(leftMostSourcePosition(exp), sp2))
        }

      case ParsedAst.Expression.UserError(sp1, sp2) =>
        WeededAst.Expression.UserError(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Bot(sp1, sp2) =>
        val ident = Name.Ident(sp1, "⊥", sp2)
        val namespace = Name.NName(sp1, List.empty, sp2)
        val name = Name.QName(sp1, namespace, ident, sp2)
        val lambda = WeededAst.Expression.Var(name, mkSL(sp1, sp2))
        WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Expression.Top(sp1, sp2) =>
        val ident = Name.Ident(sp1, "⊤", sp2)
        val namespace = Name.NName(sp1, List.empty, sp2)
        val name = Name.QName(sp1, namespace, ident, sp2)
        val lambda = WeededAst.Expression.Var(name, mkSL(sp1, sp2))
        WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess

    }
  }

  object Patterns {

    /**
      * Weeds the given pattern.
      */
    def weed(pat0: ParsedAst.Literal): Validation[WeededAst.Pattern, WeederError] = pat0 match {
      case ParsedAst.Literal.Unit(sp1, sp2) => WeededAst.Pattern.Unit(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.True(sp1, sp2) => WeededAst.Pattern.True(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.False(sp1, sp2) => WeededAst.Pattern.False(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.Char(sp1, lit, sp2) => WeededAst.Pattern.Char(lit(0), mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Float32(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Float64(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int8(sp1, sign, digits, sp2) =>
        toInt8(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int8(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int16(sp1, sign, digits, sp2) =>
        toInt16(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int16(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) =>
        toInt32(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int32(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int64(sp1, sign, digits, sp2) =>
        toInt64(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int64(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.BigInt(sp1, sign, digits, sp2) =>
        toBigInt(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.BigInt(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Pattern.Str(lit, mkSL(sp1, sp2)).toSuccess
    }

    /**
      * Compiles a parsed pattern into a weeded pattern.
      */
    def weed(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
      /*
       *  Check for non-linear pattern, i.e. if a variable occurs multiple times.
       */
      val seen = mutable.Map.empty[String, Name.Ident]

      /*
       * Local visitor.
       */
      def visit(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = pattern match {
        case ParsedAst.Pattern.Wild(sp1, sp2) => WeededAst.Pattern.Wild(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.Var(sp1, ident, sp2) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.Pattern.Var(ident, mkSL(sp1, sp2)).toSuccess
          case Some(otherIdent) =>
            NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
        }

        case ParsedAst.Pattern.Lit(sp1, lit, sp2) => weed(lit)

        case ParsedAst.Pattern.Tag(sp1, enum, tag, o, sp2) =>
          /*
           * Introduce implicit unit, if needed.
           */
          o match {
            case None =>
              val loc = mkSL(sp1, sp2)
              val lit = WeededAst.Pattern.Unit(loc)
              WeededAst.Pattern.Tag(enum, tag, lit, loc).toSuccess
            case Some(pat) => visit(pat) map {
              case p => WeededAst.Pattern.Tag(enum, tag, p, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Pattern.Tuple(sp1, pats, sp2) =>
          /*
           * Rewrites empty tuples to Unit and eliminate single-element tuples.
           */
          @@(pats map visit) map {
            case Nil => WeededAst.Pattern.Unit(mkSL(sp1, sp2))
            case x :: Nil => x
            case xs => WeededAst.Pattern.Tuple(xs, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FNone(sp1, sp2) =>
          WeededAst.Pattern.FNone(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.FSome(sp1, pat, sp2) =>
          visit(pat) map {
            case p => WeededAst.Pattern.FSome(p, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FNil(sp1, sp2) =>
          WeededAst.Pattern.FNil(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.FList(pat1, pat2, sp2) =>
          @@(weed(pat1), weed(pat2)) map {
            case (hd, tl) => WeededAst.Pattern.FList(hd, tl, mkSL(pat1.leftMostSourcePosition, sp2))
          }

        case ParsedAst.Pattern.FVec(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map(visit))
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FVec(es, r, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FSet(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map(visit))
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FSet(es, r, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FMap(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map {
            case (key, value) => @@(visit(key), visit(value))
          })
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FMap(es, r, mkSL(sp1, sp2))
          }
      }

      visit(pattern)
    }
  }


  object Predicate {

    object Head {

      /**
        * Weeds the given head predicate.
        */
      def weed(past: ParsedAst.Predicate, aliases: Map[String, ParsedAst.Predicate.Equal] = Map.empty): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case ParsedAst.Predicate.True(sp1, sp2) => WeededAst.Predicate.Head.True(mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Predicate.False(sp1, sp2) => WeededAst.Predicate.Head.False(mkSL(sp1, sp2)).toSuccess
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(t => Term.Head.toTerm(t, aliases))) map {
            case terms => WeededAst.Predicate.Head.Table(p.name, terms, mkSL(p.sp1, p.sp2))
          }

        case ParsedAst.Predicate.Equal(sp1, ident, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.Loop(sp1, ident, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.NotEqual(sp1, ident1, ident2, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
      }

    }

    object Body {

      /**
        * Weeds the given body predicate.
        */
      def weed(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case ParsedAst.Predicate.True(sp1, sp2) => Unsupported("'true' predicate is not allowed in the body of a rule.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.False(sp1, sp2) => Unsupported("'false' predicate is not allowed in the body of a rule.", mkSL(sp1, sp2)).toFailure
        case p: ParsedAst.Predicate.Ambiguous =>
          @@(p.terms.map(Term.Body.toTerm)) map {
            case terms => WeededAst.Predicate.Body.Ambiguous(p.name, terms, mkSL(p.sp1, p.sp2))
          }

        case p: ParsedAst.Predicate.NotEqual =>
          WeededAst.Predicate.Body.NotEqual(p.ident1, p.ident2, mkSL(p.sp1, p.sp2)).toSuccess

        case p: ParsedAst.Predicate.Loop => Term.Head.toTerm(p.term, Map.empty) map {
          case term => WeededAst.Predicate.Body.Loop(p.ident, term, mkSL(p.sp1, p.sp2))
        }

        case p: ParsedAst.Predicate.Equal => throw InternalCompilerException("Alias predicate should already have been eliminated.")
      }
    }

  }

  object Term {

    object Head {

      /**
        * Compiles the parsed expression `exp` into a weeded head term.
        *
        */
      def toTerm(exp: ParsedAst.Expression, aliases: Map[String, ParsedAst.Predicate.Equal]): Validation[WeededAst.Term.Head, WeederError] = exp match {
        case ParsedAst.Expression.Wild(sp1, sp2) => IllegalHeadTerm("Wildcards may not occur here.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Expression.Var(sp1, name, sp2) if name.isUnqualified => WeededAst.Term.Head.Var(name.ident, mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Expression.Var(sp1, name, sp2) =>
          IllegalHeadTerm("Qualified variable may not occur here.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Expression.Lit(sp1, lit, sp2) => Literals.compile(lit) map {
          case l => WeededAst.Term.Head.Lit(l, mkSL(sp1, sp2))
        }
        case ParsedAst.Expression.Apply(lambda, args, sp2) => lambda match {
          case ParsedAst.Expression.Var(_, name, _) =>
            val sp1 = leftMostSourcePosition(lambda)
            @@(args map (a => toTerm(a, aliases))) flatMap {
              case as => WeededAst.Term.Head.Apply(name, as, mkSL(sp1, sp2)).toSuccess
            }
          case _ => throw InternalCompilerException("Illegal head term. But proper error messages not yet implemented.")
        }
        case ParsedAst.Expression.Tag(sp1, enum, tag, o, sp2) => o match {
          case None =>
            val loc = mkSL(sp1, sp2)
            val lit = WeededAst.Literal.Unit(loc)
            val term = WeededAst.Term.Head.Lit(lit, loc)
            WeededAst.Term.Head.Tag(enum, tag, term, mkSL(sp1, sp2)).toSuccess
          case Some(e) => toTerm(e, aliases) map {
            case t => WeededAst.Term.Head.Tag(enum, tag, t, mkSL(sp1, sp2))
          }
        }
        case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
          @@(elms.map(e => toTerm(e, aliases))) map {
            case Nil => WeededAst.Term.Head.Lit(WeededAst.Literal.Unit(mkSL(sp1, sp2)), mkSL(sp1, sp2))
            case t :: Nil => t
            case es => WeededAst.Term.Head.Tuple(es, mkSL(sp1, sp2))
          }
        case _ => throw InternalCompilerException("Illegal head term. But proper error messages not yet implemented.")
      }

    }

    object Body {

      /**
        * Compiles the parsed expression `exp` into a weeded body term.
        *
        */
      def toTerm(exp: ParsedAst.Expression): Validation[WeededAst.Term.Body, WeederError] = exp match {
        case ParsedAst.Expression.Wild(sp1, sp2) => WeededAst.Term.Body.Wild(mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Expression.Var(sp1, name, sp2) if name.isUnqualified => WeededAst.Term.Body.Var(name.ident, mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Expression.Var(sp1, name, sp2) => IllegalBodyTerm("Qualified variable may not occur here.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Expression.Lit(sp1, lit, sp2) => Literals.compile(lit) map {
          case l => WeededAst.Term.Body.Lit(l, mkSL(sp1, sp2))
        }
        case ParsedAst.Expression.Apply(lambda, args, sp2) => IllegalBodyTerm("Functions call may not occur here.", mkSL(leftMostSourcePosition(lambda), sp2)).toFailure
        case _ => throw InternalCompilerException("Illegal body term. But proper error messages not yet implemented.")
      }

    }

  }

  object Annotations {
    /**
      * Weeds the given sequence of parsed annotation `xs`.
      */
    def weed(xs: Seq[ParsedAst.Annotation]): Validation[Ast.Annotations, WeederError] = {
      // collect seen annotations.
      val seen = mutable.Map.empty[String, ParsedAst.Annotation]

      // loop through each annotation.
      val result = xs.toList map {
        case x => seen.get(x.name) match {
          case None =>
            seen += (x.name -> x)
            Annotations.weed(x)
          case Some(otherAnn) =>
            DuplicateAnnotation(x.name, mkSL(otherAnn.sp1, otherAnn.sp2), mkSL(x.sp1, x.sp2)).toFailure
        }
      }
      @@(result) map Ast.Annotations
    }

    /**
      * Weeds the given parsed annotation `past`.
      */
    def weed(past: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = {
      val loc = mkSL(past.sp1, past.sp2)
      past.name match {
        case "associative" => Ast.Annotation.Associative(loc).toSuccess
        case "commutative" => Ast.Annotation.Commutative(loc).toSuccess
        case "monotone" => Ast.Annotation.Monotone(loc).toSuccess
        case "strict" => Ast.Annotation.Strict(loc).toSuccess
        case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
        case "unsafe" => Ast.Annotation.Unsafe(loc).toSuccess
        case _ => IllegalAnnotation(past.name, loc).toFailure
      }
    }
  }


  /**
    * Attempts to parse the given float32 with `sign` digits `before` and `after` the comma.
    */
  def toFloat32(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Float, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toFloat.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  def toFloat64(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Double, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toDouble.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  def toInt8(sign: Boolean, digits: String, loc: SourceLocation): Validation[Byte, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toByte.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  def toInt16(sign: Boolean, digits: String, loc: SourceLocation): Validation[Short, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toShort.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  def toInt32(sign: Boolean, digits: String, loc: SourceLocation): Validation[Int, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toInt.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  def toInt64(sign: Boolean, digits: String, loc: SourceLocation): Validation[Long, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toLong.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  def toBigInt(sign: Boolean, digits: String, loc: SourceLocation): Validation[BigInteger, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    new BigInteger(s).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Alias for SourceLocation.mk
    */
  private def mkSL(sp1: SourcePosition, sp2: SourcePosition): SourceLocation = SourceLocation.mk(sp1, sp2)

  /**
    * Returns the left most source position in the sub-tree of the expression `e`.
    */
  def leftMostSourcePosition(e: ParsedAst.Expression): SourcePosition = e match {
    case ParsedAst.Expression.Wild(sp1, _) => sp1
    case ParsedAst.Expression.Var(sp1, _, _) => sp1
    case ParsedAst.Expression.Lit(sp1, _, _) => sp1
    case ParsedAst.Expression.Apply(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Infix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.ExtendedBinary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Switch(sp1, _, _) => sp1
    case ParsedAst.Expression.Tag(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.FNone(sp1, _) => sp1
    case ParsedAst.Expression.FSome(sp1, _, _) => sp1
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FList(hd, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FVec(sp1, _, _) => sp1
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.GetIndex(sp1, _, _, _) => sp1
    case ParsedAst.Expression.PutIndex(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Existential(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Universal(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Ascribe(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.UserError(sp1, _) => sp1
    case ParsedAst.Expression.Bot(sp1, sp2) => sp1
    case ParsedAst.Expression.Top(sp1, sp2) => sp1
  }

  /**
    * Checks that no attributes are repeated.
    */
  private def checkDuplicateAttribute(attrs: Seq[Ast.Attribute]): Validation[List[Ast.Attribute], WeederError] = {
    val seen = mutable.Map.empty[String, Name.Ident]
    @@(attrs.map {
      case attr@Ast.Attribute(id, tpe) => seen.get(id.name) match {
        case None =>
          seen += (id.name -> id)
          attr.toSuccess
        case Some(otherIdent) =>
          DuplicateAttribute(id.name, otherIdent.loc, id.loc).toFailure
      }
    })
  }

  /**
    * Checks that no formal parameters are repeated.
    */
  private def checkDuplicateFormal(params: Seq[Ast.FormalParam]): Validation[List[Ast.FormalParam], WeederError] = {
    val seen = mutable.Map.empty[String, Name.Ident]
    @@(params.map {
      case formal@Ast.FormalParam(id, t) => seen.get(id.name) match {
        case None =>
          seen += (id.name -> id)
          formal.toSuccess
        case Some(otherIdent) =>
          DuplicateFormal(id.name, otherIdent.loc, id.loc).toFailure
      }
    })
  }

}
