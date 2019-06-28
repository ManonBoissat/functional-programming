package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._
import Arithmetic._

object balelec2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(468); 
 
 /**
   * This function takes a list of constraint, and returns a
   * constraint that is true if and only if at most max
   * of them are true.
   */
  def positivesLessEquals(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositives(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  };System.out.println("""positivesLessEquals: (ns: List[cafesat.api.Formulas.Formula], max: Int)cafesat.api.Formulas.Formula""");$skip(614); 

  /**
   * This function takes a list of constraints, and returns a pair.
   * The first element of the pair is a list of constraints representing the bitwise
   * sum of the constraints of `ns`.
   * The second element is a set of additional constraints that have been gathered along
   * the way. Hint: see `adder` for understanding how to use additional constraints
   */
  def countPositives(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    ns.foldLeft((List[Formula](false), Set[Formula]())) { case ((tmpSum, tmpAcc), n) =>
      val (r, c) = adder(tmpSum, List(n))
      (r, tmpAcc ++ c)
    }
  }
 
  // ======================================================================================
  
  case class Volunteer(name: String) {
    override def toString = name
  }
  case class Task(name: String, capacity: Int) {
    override def toString = name
  };System.out.println("""countPositives: (ns: List[cafesat.api.Formulas.Formula])(List[cafesat.api.Formulas.Formula], Set[cafesat.api.Formulas.Formula])""");$skip(291); 
  
  val v1 = Volunteer("Marc");System.out.println("""v1  : constraints.balelec2.Volunteer = """ + $show(v1 ));$skip(29); 
  val v2 = Volunteer("Jean");System.out.println("""v2  : constraints.balelec2.Volunteer = """ + $show(v2 ));$skip(28); 
  val v3 = Volunteer("Dom");System.out.println("""v3  : constraints.balelec2.Volunteer = """ + $show(v3 ));$skip(31); 
  val v4 = Volunteer("Fabien");System.out.println("""v4  : constraints.balelec2.Volunteer = """ + $show(v4 ));$skip(29); 
  
  val t1 = Task("Bar", 1);System.out.println("""t1  : constraints.balelec2.Task = """ + $show(t1 ));$skip(28); 
  val t2 = Task("Scene", 1);System.out.println("""t2  : constraints.balelec2.Task = """ + $show(t2 ));$skip(31); 
  val t3 = Task("Lumieres", 1);System.out.println("""t3  : constraints.balelec2.Task = """ + $show(t3 ));$skip(26); 
  val t4 = Task("Son", 2);System.out.println("""t4  : constraints.balelec2.Task = """ + $show(t4 ));$skip(60); 
  
  val volunteers: List[Volunteer] = List(v1, v2, v3, v4);System.out.println("""volunteers  : List[constraints.balelec2.Volunteer] = """ + $show(volunteers ));$skip(43); 
	val tasks: List[Task] = List(t1,t2,t3,t4);System.out.println("""tasks  : List[constraints.balelec2.Task] = """ + $show(tasks ));$skip(239); 
	val availability: Map[Volunteer, List[Task]] = Map(
		v1 -> List(t1), // Marc -> Bar
		v2 -> List(t1,t2,t3), // Jean -> Bar, Scene, Lumières
		v3 -> List(t3, t4), // Dom -> Lumières, Son
		v4 -> List(t3, t4) // Fabien -> Lumières, Son
	);System.out.println("""availability  : Map[constraints.balelec2.Volunteer,List[constraints.balelec2.Task]] = """ + $show(availability ));$skip(26); 
	val maxWorkload: Int = 2;System.out.println("""maxWorkload  : Int = """ + $show(maxWorkload ));$skip(233); 

	// generates one propositional variable per volunteer/task combination
	val varsMatrixTV: Map[(Volunteer, Task), PropVar] =
	  volunteers.flatMap({ case v@Volunteer(name) =>
	    tasks.map(t => (v, t) -> propVar(name))
	  }).toMap;System.out.println("""varsMatrixTV  : Map[(constraints.balelec2.Volunteer, constraints.balelec2.Task),cafesat.api.Formulas.PropVar] = """ + $show(varsMatrixTV ))}
}
