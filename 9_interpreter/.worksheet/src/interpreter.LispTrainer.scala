package interpreter

object LispTrainer {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(268); 

	// List(1, 2, 3) -> List(3, 2, 1)
  def reverse(l : List[Int]) : List[Int] = {

  	def iter(l: List[Int], acc: List[Int]): List[Int] = {
  		if(l.isEmpty) acc
  		else iter(l.tail, l.head :: acc)
  	}
  	
  	iter(l, Nil)
  };System.out.println("""reverse: (l: List[Int])List[Int]""");$skip(534); 
  
  /*
  (def (reverse L acc) (
      if (null? L) acc
      (reverse (cdr L) (cons (car L) acc))
    )
  )
  */
  
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // (c, b, a)
  // (a, b-a, c-b)
  // You might find useful to define an inner loop def
  def differences(l: List[Int]): List[Int] = {
  	
  	def loop(l:List[Int], acc:List[Int]):List[Int] = {
  		if(l.tail.isEmpty) l.head :: acc
  		else loop(l.tail, (l.head-l.tail.head) :: acc)
  	}
  	if(l.isEmpty) Nil
  	else loop(reverse(l), Nil)
  };System.out.println("""differences: (l: List[Int])List[Int]""");$skip(717); 
  
  /*
  def (differences L) (
			def (loop L acc) (
				if (null? (cdr L)) (cons (cdr L) acc)
				(loop (cdr L) (cons (- car L (car (cdr L))) acc))
			)
			
			if(null? L) nil
			loop((reverse L Nil) Nil)
	)

	// differences (cons 1 (cons 2 (cons 3 nil)))
	*/
  
  // List(a, b, c, d)
  // differences(l) -> List(a, b-a, c-b, d-c)
  // rebuildList(differences(l)) -> List(a, (b-a)+a, (c-b)+(b-a)+a, (d-c)+(c-b)+(b-a)+a)
  def rebuildList(l: List[Int]): List[Int] = {
  
  	def loop(l:List[Int], acc:List[Int]):List[Int] = {
  		if(l.tail.isEmpty) acc
  		else if(acc.isEmpty) loop(l, l.head :: acc)
  		else loop(l.tail, acc ::: (l.tail.head+acc.last) :: Nil)
  	}
  	
  	if(l.isEmpty) Nil
  	else loop(l, Nil)
  };System.out.println("""rebuildList: (l: List[Int])List[Int]""");$skip(27); 
  
  val l = List(1,5,8,3);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(17); val res$0 = 
  differences(l);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(30); val res$1 = 
  rebuildList(differences(l));System.out.println("""res1: List[Int] = """ + $show(res$1))}
  // List(1, 4+1=5, 3+5
  /*
  (def reverse
	  (lambda (L acc) (
	    if (null? L) acc
	    ( reverse (cdr L) (cons (car L) acc) )
	  ))
	  (reverse (1 2 3) nil)
	)
	
	
	
	(def (differences L)( def (loop L acc) (if(null? (cdr L)) (cons (cdr L) acc) (loop (cdr L) (cons (- (car L) (car (cdr L))) acc))) if(null? L) nil loop(L nil)))
	
  */
  
}
