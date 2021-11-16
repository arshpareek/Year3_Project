/*
Lexical analyser based on bit-sequence algorithm designed by Sulzmann and Lu.

I verify that I am the sole author of the source code contained in this file, except where explicitly stated to the contrary.

Source code in this file has been adapted from the paper "POSIX Regular Expression Parsing with
Derivatives" by Sulzmann and Lu unless stated otherwise.

Author: Arshdeep Singh Pareek.
Date: April 9, 2021
*/

import scala.language.implicitConversions    
import scala.language.reflectiveCalls 
import scala.annotation.tailrec   

// Implicit functions to convert 0 to false, 1 to true, and vice versa. This helps in code readability.
implicit def IntToBoolean(i:Int) = if (i == 0) false else true
implicit def BooleanToInt(b:Boolean) = if (b == false) 0 else 1

// Type declaration for bitcode sequences.
type BC = List[Boolean]

// Class declarations for basic regular expressions.
// Taken from re3.sc coursework file provided in the 6CCS3CFL module.
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp            
case class RECD(x: String, r: Rexp) extends Rexp  

// Extended regular expression to match a collection of characters/symbols.
def RANGE(ls: List[Char]) : Rexp = ls match{
  case Nil => ZERO
  case c::Nil => CHAR(c)
  case c::cs => ALT(CHAR(c), RANGE(cs))
} 

// Extended regular expression equivalent to r•r*
def PLUS(r: Rexp) : Rexp = SEQ(r, STAR(r))

// Extended regular expression equivalent to 1 + r
def OPTIONAL(r: Rexp) = ALT(r, ONE)

// Extended regular expression equivalent to r sequenced with itself n times.
// Replace by an explicit constructor of the NTIMES regular expression to improve efficiency.
//def NTIMES(r: Rexp, n: Int) : Rexp = if(n==0) ONE else SEQ(r, NTIMES(r, n-1))

// Class declarations for basic regular expressions with annotations included.
abstract class ARexp
case object AEMPTY extends ARexp
case object AZERO extends ARexp
case class AONE(ann: BC) extends ARexp
case class ACHAR(ann: BC, c: Char) extends ARexp
case class AALT(ann: BC, rs: List[ARexp]) extends ARexp   
case class ASEQ(ann: BC, r1: ARexp, r2: ARexp) extends ARexp   
case class ANTIMES(ann: BC, r: ARexp, n: Int) extends ARexp 
case class ASTAR(ann: BC, r: ARexp) extends ARexp  

// Class declarations for different values.
// Taken from lexer.sc coursework file provided in the 6CCS3CFL module except for NoMatch value.
// NoMatch value helps test (a*)*b and (a+a+)+b regular expressions.
abstract class Val
case object NoMatch extends Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Ntimes(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val

// Helper functions to simplify writing out regular expressions.
// Taken from re3.sc coursework file provided in the 6CCS3CFL module.
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp = 
  charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}

// Lists containing alphabets and digits.
// WHILE language definition upto line 130 is taken from my submission for coursework 2 in the 6CCS3CFL module.
val digits: List[Char] = ('0' to '9').toList
val alphabets: List[Char] = ('a' to 'z').toList
val alphabetsC: List[Char] = ('A' to 'Z').toList

// Regular expressions for various components of the WHILE language.
val KEYWORD : Rexp = "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip"
val OP: Rexp = "+" | "-" | "*" | "%" | "/" | "==" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "&&" | "||"
val UPPERCASE = RANGE(alphabetsC)
val LOWERCASE = RANGE(alphabets)
val LETTER = UPPERCASE | LOWERCASE
val SYM = LETTER | RANGE("._><;=,:\\".toList)
val DIGIT = RANGE(digits)
val PARANTHESES = RANGE("({)}".toList)
val WHITESPACE = PLUS(" " | "\n" | "\t")
val STR = "\"" ~ STAR((SYM | WHITESPACE | DIGIT)) ~ "\""
val COMMENTS = "//" ~ STAR(SYM | " " | "\t" | DIGIT) ~ OPTIONAL("\n")
val ID =  LETTER ~ STAR((LETTER | DIGIT | "_"))
val NUM = "0" | (RANGE(('1' to '9').toList) ~ STAR(DIGIT))
val SEMI: Rexp = ";"

// Combined regular expression for the WHILE language.
val WHILE_REGS = (("k" $ KEYWORD) | 
                  ("i" $ ID) | 
                  ("o" $ OP) | 
                  ("n" $ NUM) | 
                  ("s" $ SEMI) | 
                  ("str" $ STR) |
                  ("sym" $ SYM) |
                  ("c" $ COMMENTS) |
                  ("p" $ PARANTHESES) | 
                  ("w" $ WHITESPACE)).%

// Concatenates any given bit sequence to the existing annotation of an 
// annotated regular expression.
def fuse(bs: BC, r: ARexp) : ARexp = r match{
  case AZERO => AZERO
  case AONE(a) => AONE(bs:::a)
  case ACHAR(a, c) => ACHAR(bs:::a, c)
  case AALT(a, rs) => AALT(bs:::a, rs)
  case ASEQ(a, r1, r2) => ASEQ(bs:::a, r1, r2)
  case ASTAR(a, rs) => ASTAR(bs:::a, rs)
  case ANTIMES(a, r, n) => ANTIMES(bs:::a, r, n)
}

// Removes ZERO regular expressions from alternative regular expressions.
def flatten(rs: List[ARexp]) : List[ARexp] = rs match {
    case Nil => Nil
    case(AZERO::rs) => flatten(rs)
    case(AALT(a, r)::rs) => r.map(s => fuse(a, s)) ::: flatten(rs)
    
    case(r::rs) => r::flatten(rs)
}

// Inserts an empty bit sequence to a regular expression, converting it to 
// an annotated regular expression.
def internalize(r: Rexp) : ARexp = r match{
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALT(Nil, (List(fuse(List[Boolean](0), internalize(r1)), fuse(List[Boolean](1), internalize(r2)))))
  case SEQ(r1, r2) => ASEQ(Nil, internalize(r1), internalize(r2))
  case STAR(rs) => ASTAR(Nil, internalize(rs))
  case NTIMES(r, n) => ANTIMES(Nil, internalize(r), n)
  case RECD(x, r) => internalize(r)
}

// Determines if a regular expression can match the empty string. 
def nullableBC(r: ARexp) : Boolean = {
  r match{
    case AZERO => false
    case AONE(_) => true
    case ACHAR(_, _) => false
    case AALT(a, r::Nil) => nullableBC(r)
    case AALT(a, r::rs) => nullableBC(r) || nullableBC(AALT(a, rs))
    case ASEQ(a, r1, r2) => nullableBC(r1) && nullableBC(r2)
    case ASTAR(_, _) => true
    case ANTIMES(a, r, n) => if(n == 0) true else nullableBC(r)
  }
}

// Checks if a regular expression can match no string (the ZERO regular expression).
def isPhi(r: ARexp) : Boolean = r match{
  case ASTAR(a, rs) => false
  case ASEQ(a, r1, r2) => isPhi(r1) || isPhi(r2)
  case AALT(a, Nil) => true
  case AALT(a, r::Nil) => isPhi(r)
  case AALT(a, r::rs) => isPhi(r) && isPhi(AALT(a, rs))
  case ACHAR(a, c) => false
  case AONE(a) => false
  case AZERO => true
}


// Returns the underlying matched string under the given value.
// Taken from my submission for coursework 2 in the 6CCS3CFL module.
def flattenVal(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flattenVal(v)
  case Right(v) => flattenVal(v)
  case Sequ(v1, v2) => flattenVal(v1) ++ flattenVal(v2)
  case Stars(vs) => vs.map(flattenVal).mkString
  case Ntimes(vs) => vs.map(flattenVal).mkString
  case Rec(x, v) => flattenVal(v)
}

// Returns a list of tokens extracted from RECD regular expressions.
// Taken from my submission for coursework 2 in the 6CCS3CFL module.
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Ntimes(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flattenVal(v))::env(v)
}

// Converts annotated regular expression to unannotated regular expressions.
// Taken from code provided by Dr. Urban.
def deannotate(r: ARexp): Rexp = r match{
  case AZERO => ZERO
  case AONE(_) => ONE
  case ACHAR(bs, c) => CHAR(c)
  case AALT(bs, Nil) => ZERO
  case AALT(bs, r::Nil) => deannotate(r)
  case AALT(bs, r::rs) => ALT(deannotate(r), deannotate(AALT(bs, rs)))
  case ASEQ(bs, r1, r2) => SEQ(deannotate(r1), deannotate(r2))
  case ASTAR(cs, r) => STAR(deannotate(r))
  case ANTIMES(bs, r, n) => NTIMES(deannotate(r), n)
}

// Simplifies regular expressions in the intermediate steps of the Brzozowski matching algorithm.
// Adapted from simplification rules provided in Chengsong Tan's paper "POSIX Regular Expression Matching and Lexing".
def simpBC(r: ARexp) : ARexp = r match{
  case ASEQ(a, r1, r2) => (simpBC(r1), simpBC(r2)) match{
      case(AZERO, rs) => AZERO
      case(rs, AZERO) => AZERO
      case(AONE(as), rs) => fuse(a:::as, rs)
      case (AALT(as, rs), r2) => AALT(a:::as, rs.map(ASEQ(Nil, _, r2)))
      case(r1s, r2s) => ASEQ(a, r1s, r2s)
  }
  case AALT(a, rs) => flatten(rs.map(r => simpBC(r))).distinctBy(deannotate) match{
      case Nil => AZERO
      case r::Nil => fuse(a, r)
      case rs => AALT(a, rs)
  }
  case r => r
}

// Extracts bit-sequence from the given regular expression and appends bits 
// to show how the regular expression matches the empty string.
def mkepsBC(r: ARexp) : BC = r match{
  case AONE(a) => a
  case AALT(a, r::Nil) => a:::mkepsBC(r) 
  case AALT(a, r::rs) => if (nullableBC(r)) a:::mkepsBC(r)
                          else mkepsBC(AALT(a, rs))
  case ASEQ(a, r1, r2) => a:::mkepsBC(r1):::mkepsBC(r2)
  case ASTAR(a, rs) => a:::(List[Boolean](1))
  case ANTIMES(a, rs, n) => a:::(List.fill(n)(mkepsBC(rs)).flatten)
}

// Returns the derivative of the input annotated regular expression with respect to the input character.
def derBC(c: Char, r: ARexp) : ARexp = r match{
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(a, d) => if(c == d) AONE(a) else AZERO
  case AALT(a, rs) => AALT(a, rs.map(r => derBC(c, r)))
  case ASEQ(a, r1, r2) => if(nullableBC(r1)) AALT(a, List(ASEQ(Nil, derBC(c, r1), r2), fuse(mkepsBC(r1), derBC(c, r2))))
                          else ASEQ(a, derBC(c, r1), r2)
  case ANTIMES(a, r, n) => if(n==0) AZERO else ASEQ(a, derBC(c, r), ANTIMES(Nil, r, n-1))
  case ASTAR(a, rs) => ASEQ(a, fuse(List[Boolean](0), derBC(c, rs)), ASTAR(Nil, rs))
}

// Recursively applies the derivative to a regular expression with respect to a given string
// and simplifies intermediate regular expressions.
@tailrec
def simpDersBC(s: List[Char], r: ARexp) : ARexp = s match{
    case Nil => r
    case c::s => simpDersBC(s, simpBC(derBC(c, r)))
}

// Converts Sulzmann and Lu values to bit code sequences.
def encode(r: Rexp, v: Val) : BC = (r, v) match{
  case (ONE, Empty) => Nil
  case (CHAR(_), Chr(_)) => Nil
  case (ALT(r1, r2), Left(v)) => 0::encode(r1, v)
  case (ALT(r1, r2), Right(v)) => 1::encode(r2, v)
  case (SEQ(r1, r2), Sequ(v1, v2)) => encode(r1, v1):::encode(r2, v2)
  case (STAR(rs), Empty) => List(1)
  case (STAR(rs), Stars(v::vs)) => 0::encode(r, v):::encode(STAR(rs), Stars(vs))
  case (RECD(x1, r), Rec(x2, v)) => encode(r, v)
}

// Converts input bit-sequences and input regular expression to values.
// In this method false = 0 and true = 1.
def decode(r: Rexp, bs: BC) : (Val, BC) = (r, bs) match{
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), false::bs) => val (v, p) = decode(r1, bs)
                              (Left(v), p)
  case (ALT(r1, r2), true::bs) => val (v, p) = decode(r2, bs)
                              (Right(v), p)
  case (SEQ(r1, r2), bs) => val (v1, p1) = decode(r1, bs)
                            val (v2, p2) = decode(r2, p1)
                            (Sequ(v1, v2), p2)
  case (STAR(rs), false::b) => val (v, p1) = decode(rs, b)
                           val (vs, p2) = decode(STAR(rs), p1)
                           val Stars(s) = vs
                           (Stars(v::s), p2)
  case (STAR(rs), true::b) => (Stars(Nil), b)
  case (STAR(rs), _) => (Stars(Nil), bs)
  case (NTIMES(rs, 0), bs) => (Ntimes(Nil), bs)
  case (NTIMES(rs, n), bs) => 
                           val (v, p1) = decode(rs, bs)
                           val (vs, p2) = decode(NTIMES(rs, n-1), p1)
                           val Ntimes(s) = vs
                           (Ntimes(v::s), p2)
  case (RECD(x, r), bs) => val (v, p) = decode(r, bs)
                          (Rec(x, v), p)
}

// Decodes and tokenizes an input value based on input bit-sequence.
// Tail-recursive version of decode using an accumulator string.
// Adapted from code provided by Dr. Urban.
// First three lines of pattern matching improved to work correctly in edge cases.
@tailrec
def sdecode_aux(rs: List[Rexp], bs: BC, acc: List[String]) : List[String] = (rs, bs) match {
  case (Nil, _) => acc
  case (ALT(r1, r2)::rest, Nil) => acc
  case (STAR(r1)::rest, Nil) => acc
  case (ONE::rest, bs) => sdecode_aux(rest, bs, acc)
  case (CHAR(c)::rest, bs) => 
    sdecode_aux(rest, bs, (acc.head ++ c.toString)::acc.tail)
  case (ALT(r1, r2)::rest, false::bs) => sdecode_aux(r1::rest, bs, acc)
  case (ALT(r1, r2)::rest, true::bs) => sdecode_aux(r2::rest, bs, acc)
  case (SEQ(r1, r2)::rest, bs) => sdecode_aux(r1::r2::rest, bs, acc)
  case (STAR(r1)::rest, false::bs) => sdecode_aux(r1::STAR(r1)::rest, bs, acc)
  case (STAR(_)::rest, true::bs) => sdecode_aux(rest, bs, acc)
  case (NTIMES(r1, 0)::rest, bs) => sdecode_aux(rest, bs, acc)
  case (NTIMES(r1, n)::rest, bs) => sdecode_aux(r1::NTIMES(r1, n-1)::rest, bs, acc)
  case (RECD(s, r1)::rest, bs) => 
    sdecode_aux(r1::rest, bs, s"$s:"::acc)
}

def sdecode(r: Rexp, bs: BC) : List[String] = 
  sdecode_aux(List(r), bs, List("")).reverse

// Returns the size or the number of nodes in an annotated regular expression.
// Adapted from re3.sc coursework file from 6CCS3CFL module.
def sizeBC(r: ARexp) : Int = r match {
  case AZERO => 1
  case AONE(_) => 1
  case ACHAR(_, _) => 1
  case AALT(a, Nil) => 1
  case AALT(a, rs) => 1 + rs.map(sizeBC(_)).sum
  case ASEQ(_, r1, r2) => 1 + sizeBC(r1) + sizeBC(r2)
  case ASTAR(_, r) => 1 + sizeBC(r)
  case ANTIMES(_, r, n) => 1 + sizeBC(r)
}

// Returns the size or the number of nodes in a regular expression.
// Taken from re3.sc coursework file from 6CCS3CFL module.
def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, n) => size(r)
  case RECD(_, r) => 1 + size(r)
}

// Returns a value associated with matching the input string s with respect 
// to the input regular expression r. It needs further processing for tokenisation.
def blexer_simp(r: Rexp, s: List[Char]) : Val = {
    val start = System.nanoTime()
  
    val a = simpDersBC(s, internalize(r))
    
    val end = System.nanoTime()
    //Used to collect data for the experiments.
    //println("derTime " + (end - start)/(1.0e9))
    //println("simpsize: " + size(a))
    //println("simplified re: " + a)
    if(nullableBC(a)) {
        val start = System.nanoTime()
        val outVal = decode(r, mkepsBC(a))._1
        val end = System.nanoTime()
        println("decodeTime " + (end - start)/(1.0e9))
        outVal
    } 
    else { 
      println("No match was found.")
      NoMatch
    } 
}

// Helper function to measure runtime of a function.
// Taken from re3.sc coursework base file from the 6CCS3CFL module.
def runtime[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

def blexer2_simp(r: Rexp, s: String) : List[String] = {
  val a = simpDersBC(s.toList, internalize(r))
  //Used for measuring size of a regular expression.
  //println("size: " + size(a))
  if(nullableBC(a)){
    sdecode(r, mkepsBC(a))
  }
  else{
    println("No match found.")
    List("")
  }
}


// *** THE FOLLOWING CODE IS FOR TESTING AND EXPERIMENT PURPOSES.***


//Some functions to test the correctness of important functions.
// Performs tests on the simpBC function to ensure correct output for each pattern match branch.
def simpFunctionTest() : Unit = {
  println("Simplification Test:")
  val test1 = (simpBC(AONE(Nil)) == AONE(Nil))
  println(test1)
  val test2 = (simpBC(ACHAR(Nil, 'z')) == ACHAR(Nil, 'z'))
  println(test2)
  val test3 = (simpBC(ASTAR(Nil, AONE(List(0)))) == ASTAR(Nil, AONE(List(0))))
  println(test3)
  val test4 = (simpBC(ANTIMES(List(1), AONE(List(0)), 8)) == ANTIMES(List(1), AONE(List(0)), 8))
  println(test4)
  val test5 = (simpBC(ASEQ(List(1), ACHAR(Nil, 'b'), AZERO)) == AZERO)
  println(test5)
  val test6 = (simpBC(ASEQ(List(1), AZERO, ACHAR(Nil, 'b'))) == AZERO)
  println(test6)
  val test7 = (simpBC(ASEQ(List(1), AONE(List(0, 1)), ACHAR(List(1), 'b'))) == ACHAR(List(1, 0, 1, 1), 'b'))
  println(test7)
  val test8 = (simpBC(ASEQ(List(1), AALT(List(0), List(AONE(Nil), ACHAR(Nil, 'c'))), AONE(List(0)))) == AALT(List(1, 0), List(ASEQ(Nil, AONE(Nil), AONE(List(0))), ASEQ(Nil, ACHAR(Nil, 'c'), AONE(List(0))))))
  println(test8)
  val test9 = (simpBC(AALT(List(1), Nil)) == AZERO)
  println(test9)
  val test10 = (simpBC(AALT(List(1), List(AONE(List(0))))) == AONE(List(1, 0)))
  println(test10)
  val test11 = (simpBC(AALT(List(1), List(AONE(List(0)), AONE(List(1))))) == AONE(List(1, 0)))
  println(test11)
  val test12 = (simpBC(AALT(List(1), List(AONE(List(0)), AONE(List(1)), ACHAR(Nil, 'a')))) == AALT(List(1), List(AONE(List(0)), ACHAR(Nil, 'a'))))
  println(test12)
}


// Performs tests on the mkepsBC function to ensure correct output for each pattern match branch.
def mkepsFunctionTest() : Unit = {
  println("Mkeps Test:")
  val test1 = (mkepsBC(AONE(List(0, 1))) == List[Boolean](0, 1))
  println(test1)
  val test2 = (mkepsBC(AALT(List(0, 1), List(AONE(List(1))))) == List[Boolean](0, 1, 1))
  println(test2)
  val test3 = (mkepsBC(AALT(List(0, 1), List(AONE(List(1)), ASTAR(List(1), ACHAR(Nil, 'a'))))) == List[Boolean](0, 1, 1))
  println(test3)
  val test4 = (mkepsBC(AALT(List(0, 1), List(ACHAR(Nil, 'a'), AONE(List(0))))) == List[Boolean](0, 1, 0))
  println(test4)
  val test5 = (mkepsBC(ASEQ(List(0, 1), AONE(List(1)), AONE(Nil))) == List[Boolean](0, 1, 1))
  println(test5)
  val test6 = (mkepsBC(ASTAR(List(0), ACHAR(Nil, 'a'))) == List[Boolean](0, 1))
  println(test6)
  val test7 = (mkepsBC(ANTIMES(List(0), AONE(List(0,1)), 0)) == List[Boolean](0))
  println(test7)
  val test8 = (mkepsBC(ANTIMES(List(0), AONE(List(0,1)), 1)) == List[Boolean](0, 0, 1))
  println(test8)
  val test9 = (mkepsBC(ANTIMES(List(0), AONE(List(0,1)), 5)) == List[Boolean](0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
  println(test9)
}


// Performs tests on the derBC function to ensure correct output for each pattern match branch.
def derFunctionTest() : Unit = {
  println("Derivative Test:")
  val test1 = (derBC('a', AZERO) == AZERO)
  println(test1)
  val test2 = (derBC('a', AONE(Nil)) == AZERO)
  println(test2)
  val test3 = (derBC('a', ACHAR(Nil, 'a')) == AONE(Nil))
  println(test3)
  val test4 = (derBC('a', ACHAR(Nil, 'c')) == AZERO)
  println(test4)
  val test5 = (derBC('a', AALT(List(0, 1), List(AZERO, AONE(Nil), ACHAR(Nil,'b')))) == AALT(List(0, 1), List(AZERO, AZERO, AZERO)))
  println(test5)
  val test6 = (derBC('a', AALT(List(0, 1), List(AZERO, AONE(Nil), ACHAR(Nil,'a')))) == AALT(List(0, 1), List(AZERO, AZERO, AONE(Nil))))
  println(test6)
  val test7 = (derBC('a', ASEQ(Nil, ACHAR(Nil,'a'), AONE(Nil))) == ASEQ(Nil, AONE(Nil), AONE(Nil)))
  println(test7)
  val test8 = (derBC('a', ASEQ(List(0, 1), AONE(List(0)), ACHAR(Nil,'a'))) == AALT(List(0, 1), List(ASEQ(Nil, AZERO, ACHAR(Nil, 'a')), AONE(List(0)))))
  println(test8)
  val test9 = (derBC('a', ANTIMES(Nil, ACHAR(Nil,'a'), 0)) == AZERO)
  println(test9)
  val test10 = (derBC('a', ANTIMES(Nil, ACHAR(Nil,'a'), 1)) == ASEQ(Nil, AONE(Nil), ANTIMES(Nil, ACHAR(Nil, 'a'), 0)))
  println(test10)
  val test11 = (derBC('a', ANTIMES(Nil, ACHAR(Nil,'a'), 2)) == ASEQ(Nil, AONE(Nil), ANTIMES(Nil, ACHAR(Nil, 'a'), 1)))
  println(test11)
  val test12 = (derBC('a', ASTAR(List(0), ACHAR(Nil, 'a'))) == ASEQ(List(0), AONE(List(0)), ASTAR(Nil, ACHAR(Nil, 'a'))))
  println(test12)
}

// Test function to evaluate the performance of the lexing algorithm under the regular expression (a*)*b
def doubleStarTest() = {
  println("(a*)*b: ")

  for (i <- 0 to 150 by 10) {
    println(f"$i: ${runtime(5, blexer2_simp(RECD("doubleStar", SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))), (("a" * i))))}%.5f")
  }
}

// Test function to evaluate the performance of the lexing algorithm under the regular expression (((a+)(a+))+)b
def triplePlusTest() = {
  println("(a+a+)+b: ")

  for (i <- 0 to 150 by 10) {
    println(f"$i: ${runtime(5, blexer2_simp(RECD("triplePlus", SEQ(PLUS(SEQ(PLUS(CHAR('a')), PLUS(CHAR('a')))), CHAR('b'))), (("a" * i))))}%.5f")
  }
}

// Test function to evaluate the performance of the lexing algorithm under the above regular expression.
def nTimesTest() = {
  for (i <- 0 to 100 by 5) {
      //println(blexer_simp(EVIL(i), ("a" * i).toList))
    println(f"$i: ${runtime(5, blexer2_simp(RECD("ntimes", SEQ(NTIMES(OPTIONAL(CHAR('a')), i), NTIMES(CHAR('a'), i))), ("a" * i)))}%.5f")
  }
}

// Test function to evaluate the performance of the lexing algorithm under the regular expression (a + aa)*.
def plusStarTest() = {
  for (i <- 0 to 150 by 5) {
    println(f"$i: ${runtime(5, blexer2_simp(RECD("plusStarTest", STAR(ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('a'))))), ("a" * i)))}%.5f")
  }
} 

// Sample WHILE programs for experiments and testing.
// Taken from the coursework file lexer.sc in 6CCS3CFL module.
val progFib = """write "Fib";
read n;
minus1 := 0;
minus2 := 1;
while n > 0 do {
  temp := minus2;
  minus2 := minus1 + minus2;
  minus1 := temp;
  n := n - 1
};
write "Result";
write minus2"""
// Taken from my answer to HW08 in 6CCS3CFL module.
val progFactorial = 
"""read (n);
factorial := 1;
i := 1;
while i <= n do {
 factorial := factorial * i;
 i := i + 1
};
result := factorial;
write (result)"""

// Test function to evaluate the performance of the lexing algorithm for WHILE programs.
def whileProgTest() = {
    for (i <- 0 to 50 by 5) {
        println(i)
        println(f"$i: ${runtime(5, blexer2_simp(WHILE_REGS, (progFactorial*i)))}%.5f")
    }
}


//doubleStarTest()
//whileProgTest()
//triplePlusTest()
//nTimesTest()
//plusStarTest()


//derFunctionTest()
//mkepsFunctionTest()
//simpFunctionTest()