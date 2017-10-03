package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  /** term ::= 'true'
             | 'false'
             | 'if' term 'then' term 'else' term
             | '0'
             | 'succ' term
             | 'pred' term
             | 'iszero' term
   */
  def term: Parser[Term] = "true" ^^^ True |
    "false" ^^^ False |
    nv |
    "if" ~ term ~ "then" ~ term ~ "else" ~ term ^^ {
      case ("if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3) => If(t1,t2,t3)
    } |
    "succ" ~ term ^^ {
      case ("succ" ~ t1) => Succ(t1)
    } |
    "pred" ~ term ^^ {
      case ("pred" ~ t1) => Pred(t1)
    } |
    "iszero" ~ term ^^ {
      case ("iszero" ~ t1) => IsZero(t1)
    }

  def nv: Parser[Term] =
    numericLit ^^ {
      case num if num.toInt==0 => Zero
      case num => convertToNV(num.toInt)
    } |
    "succ" ~> nv ^^ {case num => Succ(num)}

  def convertToNV(num: Int): Term = {
    var t = Succ(Zero)
    for (_ <- 2 to num) {
      t = Succ(t)
    }
    t
  }

  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Return a list of at most n terms, each being one step of reduction. */
  def path(t: Term, n: Int = 64): List[Term] =
    if (n <= 0) Nil
    else
      t :: {
        try {
          path(reduce(t), n - 1)
        } catch {
          case NoReductionPossible(t1) =>
            Nil
        }
      }

  /** Perform one step of reduction, when possible.
   *  If reduction is not possible NoReductionPossible exception
   *  with corresponding irreducible term should be thrown.
   */
  def reduce(t: Term): Term =
    t match {
      case If(True, t1, _) => t1
      case If(False, _, t2) => t2
      case IsZero(Zero) => True
      case IsZero(Succ(t1)) if isNV(t1) => False
      case Pred(Zero) => Zero
      case Pred(Succ(t1)) if isNV(t1) => t1
      case If(cond, t1, t2) => If(reduce(cond), t1, t2)
      case IsZero(t1) => IsZero(reduce(t1))
      case Pred(t1) => Pred(reduce(t1))
      case Succ(t1) => Succ(reduce(t1))
      case default => throw NoReductionPossible(t)
    }

  def isNV(t: Term): Boolean =
    t match {
      case Zero => true
      case Succ(t1) => isNV(t1)
      case default => false
    }

  case class TermIsStuck(t: Term) extends Exception(t.toString)

  /** Perform big step evaluation (result is always a value.)
   *  If evaluation is not possible TermIsStuck exception with
   *  corresponding inner irreducible term should be thrown.
   */
  def eval(t: Term): Term =
    t match {
      case If(t1,t2,t3) if eval(t1).equals(True) && isValue(eval(t2)) => eval(t2)
      case If(t1,t2,t3) if eval(t1).equals(False) && isValue(eval(t3)) => eval(t3)
      case Succ(t1) if isNV(eval(t1)) => Succ(eval(t1))
      case Pred(t1) if eval(t1).equals(Zero) => Zero
      case Pred(t1) => eval(t1) match {
        case Succ(nv) if isNV(nv) => nv
        case default => throw new TermIsStuck(t1)
      }
      case IsZero(t1) if eval(t1).equals(Zero) => True
      case IsZero(t1) => eval(t1) match {
        case Succ(nv) if isNV(nv) => False
        case default => throw new TermIsStuck(t1)
      }
      case default if isValue(t) => t
      case default => throw new TermIsStuck(t)
    }

  def isValue(t: Term): Boolean =
    t match {
      case True | False => true
      case default if isNV(t) => true
      case default => false
    }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        for (t <- path(trees))
          println(t)
        try {
          print("Big step: ")
          println(eval(trees))
        } catch {
          case TermIsStuck(t) => println("Stuck term: " + t)
        }
      case e =>
        println(e)
    }
  }
}
