package interpreter
import Lisp._

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  // TODO: Insert code for the REPL
 /*
  lisp2string(Lisp.evaluate(LispCode.reverse))
  lisp2string(Lisp.evaluate(LispCode.differences))
  
  val diffInstr = "(differences (quote(1 2 3 7)))"
  
  println("lisp > "+diffInstr)
  println(lisp2string(Lisp.evaluate(diffInstr)))
  
  
   lisp2string(Lisp.evaluate(LispCode.rebuildList))
  */
  
   val filter = """
   (def (filter p L) (
     def (loop L acc) (
       if(null? L) acc
       (
         if( p (car L) ) (loop (cdr L) (cons (car L) acc))
         (loop (cdr L) acc)
       )
     )
     (loop L nil)
   ))  
   """
   lisp2string(Lisp.evaluate(filter))
   
   // (filter (lambda (x) (= x 2)) (cons 1 (cons 2 (cons 3 nil))))
  
  var lispExpr : String = "";
  do {
    print("lisp > ")
    lispExpr = in.readLine()
    if(lispExpr equals "exit") System.exit(0)
    
    val res = lisp2string(Lisp.evaluate(lispExpr))
    println(res)
  } while(true)
  
  

}

object LispCode {
  /**
   * isEmpty -> (null? x) : return true if x isEmpty
   * head -> (car x) : return head of list x
   * tail -> (cdr x) : return tail of list x
   * Nil -> nil
   * :: -> cons
   * 
   * 
   */
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  /* version scala :
     def reverse(l : List[Int]) : List[Int] = {

      def iter(l: List[Int], acc: List[Int]): List[Int] = {
        if(l.isEmpty) acc
        else iter(l.tail, l.head :: acc)
      }
      
      iter(l, Nil)
    } 
   */
  // (reverse (cons 1 (cons 2 (cons 3 nil))) nil)
  val reverse = """
  def (reverse L acc) (
      
        if (null? L) acc
        (reverse (cdr L) (cons (car L) acc))
      
    )
  """.split("\n").mkString

  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  /* version scala : 
     def differences(l: List[Int]): List[Int] = {
      def loop(l:List[Int], acc:List[Int]):List[Int] = {
        if(l.tail.isEmpty) l.head :: acc
        else loop(l.tail, (l.head-l.tail.head) :: acc)
      }
      loop(reverse(l), Nil)
    }
   */
  val differences = """
  def (differences L) 
    (
        def (loop L acc) (
            if (null? (cdr L))
              (cons (car L) acc)
            ( loop (cdr L) (cons (- (car L) (car (cdr L))) acc) )
        )
        
        (if (null? L)
             nil
         ( loop (reverse L nil) nil )
        )
    )
  """.split("\n").mkString
  
  // ( differences (quote(1 2 3 7) ) )
  
   val rebuildList = """
  def (rebuildList L) (
    def (loop prev rem)
      (if (null? rem)
          nil
          (cons (+ (car rem) prev) (loop (+ (car rem) prev) (cdr rem)))
      )
    
    (if (null? L) L
        (cons (car L) (loop (car L) (cdr L)))
    )
  )
  """.split("\n").mkString
  
  // (rebuildList (quote(1 1 1 4)))

  // (rebuildList (differences (
  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
