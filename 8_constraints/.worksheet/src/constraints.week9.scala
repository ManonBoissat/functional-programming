package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

object week9 {
  case class Band(name: String) {
    override def toString = name
  }

  case class Stage(name: String) {
    override def toString = name
  }

  case class Time(time: String) {
    override def toString = time
  }

  type Slot = (Stage, Time);import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(406); 
  
  val s1 = Stage("Scene 1");System.out.println("""s1  : constraints.week9.Stage = """ + $show(s1 ));$skip(28); 
  val s2 = Stage("Scene 2");System.out.println("""s2  : constraints.week9.Stage = """ + $show(s2 ));$skip(279); 
  
  val preferences : Map[Band, List[Slot]] = Map(
  	Band("Vaalnor") ->	List(
  		(s1, Time("13h00")),
  		(s1, Time("16h00")),
  		(s2, Time("22h00"))
  	),
  	
  	Band("Equilibrium") -> List(
  		(s1, Time("20h00")),
  		(s1, Time("22h00")),
  		(s2, Time("22h00"))
  	)
  );System.out.println("""preferences  : Map[constraints.week9.Band,List[constraints.week9.Slot]] = """ + $show(preferences ));$skip(114); 

  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] =
    preferences.toList.flatMap(_._2).toSet;System.out.println("""getUniqueSlots: (preferences: Map[constraints.week9.Band,List[constraints.week9.Slot]])Set[constraints.week9.Slot]""");$skip(50); 
 
 	val bands: Seq[Band] = preferences.keys.toSeq;System.out.println("""bands  : Seq[constraints.week9.Band] = """ + $show(bands ));$skip(58); 
	val slots: Seq[Slot] = getUniqueSlots(preferences).toSeq;System.out.println("""slots  : Seq[constraints.week9.Slot] = """ + $show(slots ));$skip(219); 

	//generates one propositional variable per band/slot combination
	val varsMatrix: Map[(Band, Slot), PropVar] =
      bands.flatMap({ case b@Band(name) =>
        slots.map(s => (b, s) -> propVar(name))
      }).toMap;System.out.println("""varsMatrix  : Map[(constraints.week9.Band, constraints.week9.Slot),cafesat.api.Formulas.PropVar] = """ + $show(varsMatrix ));$skip(172); 
	  
  val propsByBands : Map[Band, List[PropVar]] =
  	preferences.map {
  		case (band: Band, slotList: List[Slot]) =>
  			(band, slotList.map(varsMatrix(band, _)))
  	};System.out.println("""propsByBands  : Map[constraints.week9.Band,List[cafesat.api.Formulas.PropVar]] = """ + $show(propsByBands ));$skip(163); 

 val desirableSlots: Seq[Formula] =
 	(propsByBands.toList.map{
 		case (band:Band, slots:List[PropVar]) =>
 			slots.foldLeft[Formula](false)(_ || _)
 	}).toSeq;System.out.println("""desirableSlots  : Seq[cafesat.api.Formulas.Formula] = """ + $show(desirableSlots ));$skip(265); 
           
	//A set of constraints ensuring that a band gets at most one slot
	val eachBandPlaysOnce: Seq[Formula] = (for{
		band <- bands
		slotA <- slots
		slotB <- slots
		if slotA != slotB
	} yield !(varsMatrix(band, slotA) && varsMatrix(band, slotB)) ).toSeq;System.out.println("""eachBandPlaysOnce  : Seq[cafesat.api.Formulas.Formula] = """ + $show(eachBandPlaysOnce ));$skip(167); 
	
	val eachSlotUsedOnce: Seq[Formula] = (for{
		slot <- slots
		bandA <- bands
		bandB <- bands
	} yield !(varsMatrix(bandA, slot) && varsMatrix(bandB, slot)) ).toSeq;System.out.println("""eachSlotUsedOnce  : Seq[cafesat.api.Formulas.Formula] = """ + $show(eachSlotUsedOnce ));$skip(150); 
	
	

	 //combining all the constraints together
    val allConstraints: Seq[Formula] =
      desirableSlots ++ eachBandPlaysOnce ++ eachSlotUsedOnce;System.out.println("""allConstraints  : Seq[cafesat.api.Formulas.Formula] = """ + $show(allConstraints ));$skip(121); 
  
    //finding a satisfying assignment to the constraints
    val res = solveForSatisfiability(and(allConstraints:_*));System.out.println("""res  : Option[cafesat.api.Solver.Model] = """ + $show(res ));$skip(186); val res$0 = 
  
    res.map(model => {
      bands.map(band => {
        val assignedSlot = slots.find(slot => model(varsMatrix((band, slot))))
        (band, assignedSlot.get)
      }).toMap
    })
	 
  
	
	
	
	
   import Arithmetic._;System.out.println("""res0: Option[scala.collection.immutable.Map[constraints.week9.Band,constraints.week9.Slot]] = """ + $show(res$0));$skip(353); 
 
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
  
  val v1 = Volunteer("Marc");System.out.println("""v1  : constraints.week9.Volunteer = """ + $show(v1 ));$skip(29); 
  val v2 = Volunteer("Jean");System.out.println("""v2  : constraints.week9.Volunteer = """ + $show(v2 ));$skip(28); 
  val v3 = Volunteer("Dom");System.out.println("""v3  : constraints.week9.Volunteer = """ + $show(v3 ));$skip(31); 
  val v4 = Volunteer("Fabien");System.out.println("""v4  : constraints.week9.Volunteer = """ + $show(v4 ));$skip(29); 
  
  val t1 = Task("Bar", 1);System.out.println("""t1  : constraints.week9.Task = """ + $show(t1 ));$skip(28); 
  val t2 = Task("Scene", 1);System.out.println("""t2  : constraints.week9.Task = """ + $show(t2 ));$skip(31); 
  val t3 = Task("Lumieres", 1);System.out.println("""t3  : constraints.week9.Task = """ + $show(t3 ));$skip(26); 
  val t4 = Task("Son", 2);System.out.println("""t4  : constraints.week9.Task = """ + $show(t4 ));$skip(52); 
  
  val volunteers: List[Volunteer] = List(v1, v2);System.out.println("""volunteers  : List[constraints.week9.Volunteer] = """ + $show(volunteers ));$skip(37); 
	val tasks: List[Task] = List(t1,t2);System.out.println("""tasks  : List[constraints.week9.Task] = """ + $show(tasks ));$skip(236); 
	val availability: Map[Volunteer, List[Task]] = Map(
		v1 -> List(t1), // Marc -> Bar
		v2 -> List(t2) // Jean -> Bar, Scene, Lumières
		//v3 -> List(t3, t4), // Dom -> Lumières, Son
		//v4 -> List(t3, t4) // Fabien -> Lumières, Son
	);System.out.println("""availability  : Map[constraints.week9.Volunteer,List[constraints.week9.Task]] = """ + $show(availability ));$skip(26); 
	val maxWorkload: Int = 2;System.out.println("""maxWorkload  : Int = """ + $show(maxWorkload ));$skip(247); 

   // generates one propositional variable per volunteer/task combination
    val varsMatrixTV: Map[(Volunteer, Task), PropVar] =
      volunteers.flatMap({ case v@Volunteer(name) =>
        tasks.map(t => (v, t) -> propVar(name))
      }).toMap;System.out.println("""varsMatrixTV  : Map[(constraints.week9.Volunteer, constraints.week9.Task),cafesat.api.Formulas.PropVar] = """ + $show(varsMatrixTV ));$skip(224); 
	
	val getVolunteersByTasks : Map[Task, List[Volunteer]] = tasks.map({
      case (task:Task) =>
        val listVol = availability.toList.filter { a => a._2.contains(task) }.map(x => x._1)
      (task, listVol)
   }).toMap;System.out.println("""getVolunteersByTasks  : Map[constraints.week9.Task,List[constraints.week9.Volunteer]] = """ + $show(getVolunteersByTasks ));$skip(182); 
   

	val tasksByVolunteer : Map[Volunteer, List[PropVar]] =
  	availability.map {
  		case (v: Volunteer, tasksList: List[Task]) =>
  			(v, tasksList.map(varsMatrixTV(v, _)))
  	};System.out.println("""tasksByVolunteer  : Map[constraints.week9.Volunteer,List[cafesat.api.Formulas.PropVar]] = """ + $show(tasksByVolunteer ));$skip(124); 
  	
  	val propsByTask: Map[Task, List[PropVar]] =
      tasks.map(t => t -> volunteers.map(v => varsMatrixTV(v, t))).toMap;System.out.println("""propsByTask  : Map[constraints.week9.Task,List[cafesat.api.Formulas.PropVar]] = """ + $show(propsByTask ));$skip(137); 
      
   val propsByVolunteer: Map[Volunteer, List[PropVar]] =
      volunteers.map(v => v -> tasks.map(t => varsMatrixTV(v, t))).toMap;System.out.println("""propsByVolunteer  : Map[constraints.week9.Volunteer,List[cafesat.api.Formulas.PropVar]] = """ + $show(propsByVolunteer ));$skip(157); 
      
  val maxWorkloadRespected: Seq[Formula] =
      propsByVolunteer
        .map{ case (v, ts) => positivesLessEquals(ts, maxWorkload) }
        .toSeq;System.out.println("""maxWorkloadRespected  : Seq[cafesat.api.Formulas.Formula] = """ + $show(maxWorkloadRespected ));$skip(301); 
	
	// Ensure that all Volunteers are only assigned at desirable tasks (tasks matching in availability mapping)
	val desirableTasks: Seq[Formula] =
 	(tasksByVolunteer.toList.map{
 		case (v:Volunteer, propVarTasksList:List[PropVar]) =>
 			propVarTasksList.foldLeft[Formula](false)(_ || _)
 	}).toSeq;System.out.println("""desirableTasks  : Seq[cafesat.api.Formulas.Formula] = """ + $show(desirableTasks ));$skip(272); 
	 
	  val eachTaskCapacityFullyUsed : Seq[Formula] = (for {
      task <- tasks
      volunteer <- volunteers
  
      if(getVolunteersByTasks(task).contains(volunteer) && getVolunteersByTasks(task).size >= task.capacity)
    } yield varsMatrixTV(volunteer, task) ).toSeq;System.out.println("""eachTaskCapacityFullyUsed  : Seq[cafesat.api.Formulas.Formula] = """ + $show(eachTaskCapacityFullyUsed ));$skip(194); 
    
    
    //combining all the constraints together
    val allConstraintsTV: Seq[Formula] =
      desirableTasks ++ eachTaskCapacityFullyUsed;System.out.println("""allConstraintsTV  : Seq[cafesat.api.Formulas.Formula] = """ + $show(allConstraintsTV ));$skip(125);  //++ eachVolunteerAssignedInRespectWithWorkload
  
    //finding a satisfying assignment to the constraints
    val resTV = solveForSatisfiability(and(allConstraintsTV:_*));System.out.println("""resTV  : Option[cafesat.api.Solver.Model] = """ + $show(resTV ));$skip(240); val res$1 = 
    

    
    resTV.map(model => {
        tasks.map(task => {
          val assignedVolunteer = volunteers.filter(volunteer => model.contains(varsMatrixTV((volunteer, task))))
          (task, assignedVolunteer)
        }).toMap
      });System.out.println("""res1: Option[scala.collection.immutable.Map[constraints.week9.Task,List[constraints.week9.Volunteer]]] = """ + $show(res$1));$skip(291); 
	 // Ensure that all volunteers are assigned to tasks in respect with the maximum Workload.

   val eachVolunteerAssignedInRespectWithWorkload : Seq[Formula] = availability.map{
   	case (v:Volunteer, tasksList:List[Task]) =>
   		if(tasksList.size<=maxWorkload) varsMatrixTV( (v, _) )
   };System.out.println("""eachVolunteerAssignedInRespectWithWorkload  : Seq[cafesat.api.Formulas.Formula] = """ + $show(eachVolunteerAssignedInRespectWithWorkload ));$skip(496); val res$2 = 
   
   positivesLessEquals(
  
     def putToTheSameLength(n1: List[String], n2: List[String]): (List[String], List[String]) =
  ((for (x <- List.range(0, n2.length) if x >= n1.length) yield "") ::: n1, (for (x <- List.range(0, n1.length) if x >= n2.length) yield "") ::: n2)
   
   
   val n1 = List("A0", "A1" , "A2", "A3")
   val n2 = List("B0", "B1")
   
   val n = n1.reverse.zipAll(n2.reverse, "", "").unzip
   val (vn1, vn2) = (n._1.reverse, n._2.reverse)
   
   putToTheSameLength(n1,n2);System.out.println("""res2: <error> = """ + $show(res$2))}
}
