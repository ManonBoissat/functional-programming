package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its integer representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = {
    @tailrec
    def iter(n:List[Boolean], value:Int, acc:Int):Int = n match {
      case Nil => acc
      case x :: xs =>
        iter(xs, 2*value, acc + (if(x) value else 0) )
    }
    iter(n.reverse, 1, 0)
  }
  // equivalent à :
  //n.foldRight((1,0)){ case (b:Boolean, (v:Int, acc:Int)) => (2*v, acc+(if(b) v else 0) }._2
    

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = {
    @tailrec
    def iter(n:Int, acc:List[Boolean]):List[Boolean] = n match {
      case 0 => acc
      case x if x%2 == 1 => iter( n/2, true :: acc)
      case x if x%2 == 0 => iter(n/2, false :: acc)
    }
    if(n==0) List(false) else iter(n, Nil)
  }
    
  def putToTheSameLength(n1: List[Formula], n2: List[Formula]): (List[Formula], List[Formula]) =
  ((for (x <- List.range(0, n2.length) if x >= n1.length) yield false) ::: n1, (for (x <- List.range(0, n1.length) if x >= n2.length) yield false) ::: n2)


  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    /*
    val n = n1.reverse.zipAll(n2.reverse, False, False)
    // (A3, B3), (A2, B2), (A1, B1), (A0, B0)
    n.foldRight[(Formula, Formula)]( (False, False) ){
      case ( (ltAcc:Formula, eqAcc:Formula), (b1:Formula, b2:Formula) ) =>
                
        eqAcc && !b1 && b2 || ltAcc 
        // ltAcc : formule pour less Equal
        // eqAcc : formule pour equal
    }
    
    def xi(i:Int) : Formula = {
      n(n.length-i)._1 && n(n.length-i)._2 || !n(n.length-i)._1 && !n(n.length-i)._2
    }
    
    def equal(n:List[(Formula,Formula)], i:Int) : Formula = n match {
      case (x, y) :: xs =>
        xi(i) && equal( xs , i+1)
      case Nil =>
        True
    }
     
    // equal(n, 0)
    */
    
    /*
    def inner(n1:List[Formula], n2:List[Formula]) : Formula = (n1,n2) match {
      case (x :: Nil, y :: Nil) =>
        !(x && !y)
      case (x::xs, y::ys) => 
        if(x == True && y == False) False
        else if(x==False && y == True) True
        else inner(xs, ys)
      
      case others => False

    }
    
    if(n1.size > n2.size) False 
    else if(n1.size < n2.size) True
    else inner(n1,n2)
    */
    
    val n = n1.reverse.zipAll(n2.reverse, False, False).unzip
    val (v1, v2) = (n._1.reverse, n._2.reverse)
    
    def aux(nn1: List[Formula], nn2: List[Formula], f: Formula): List[Formula] = {
      if (nn1.isEmpty) Nil
      else {
        (f && !nn1.head && nn2.head) :: aux(nn1.tail, nn2.tail, f && ((nn1.head && nn2.head) || (!nn1.head && !nn2.head))) 
      }
    }
    
    or(aux(v1, v2, true): _*) || and(v1.zip(v2).map(x => ((x._1 && x._2) || (!x._1 && !x._2))): _*)
    
  }
    

  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
    val cOut : Formula = (
      ((a xor b) && cIn) || (a && b) 
    )
    
     val S : Formula = (
      (a xor b) xor cIn
    )
    
    (cOut, S)
  }

  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
  
    // List( (A3, B3), (A2,B2), (A1,B1), (A0,B0) )
    
    @tailrec
    def iter(n:List[(Formula, Formula)], cIn:Formula, acc:(List[Formula], Set[Formula])) : (List[Formula], Set[Formula]) = n  match {
      case (a, b) :: ms =>
        val cOutProp = propVar()
        val sProp = propVar()
        val (cOut, s) = fullAdder(a, b, cIn)
        val cOutConstraint = cOutProp iff cOut
        val sConstraint = sProp iff s
        iter(ms, cOutProp, (sProp :: acc._1, acc._2 + cOutConstraint + sConstraint ) )
      case Nil =>
        (cIn :: acc._1, acc._2)
    }
    
    val n = n1.reverse.zipAll(n2.reverse, False, False)
    iter(n, False, (Nil, Set.empty))
  }

  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }


}
