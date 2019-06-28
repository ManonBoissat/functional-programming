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

  type Slot = (Stage, Time)
  
  val s1 = Stage("Scene 1")
  val s2 = Stage("Scene 2")
  
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
  )

  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] =
    preferences.toList.flatMap(_._2).toSet
 
 	val bands: Seq[Band] = preferences.keys.toSeq
	val slots: Seq[Slot] = getUniqueSlots(preferences).toSeq

	//generates one propositional variable per band/slot combination
	val varsMatrix: Map[(Band, Slot), PropVar] =
      bands.flatMap({ case b@Band(name) =>
        slots.map(s => (b, s) -> propVar(name))
      }).toMap
	  
  val propsByBands : Map[Band, List[PropVar]] =
  	preferences.map {
  		case (band: Band, slotList: List[Slot]) =>
  			(band, slotList.map(varsMatrix(band, _)))
  	}

 val desirableSlots: Seq[Formula] =
 	(propsByBands.toList.map{
 		case (band:Band, slots:List[PropVar]) =>
 			slots.foldLeft[Formula](false)(_ || _)
 	}).toSeq
           
	//A set of constraints ensuring that a band gets at most one slot
	val eachBandPlaysOnce: Seq[Formula] = (for{
		band <- bands
		slotA <- slots
		slotB <- slots
		if slotA != slotB
	} yield !(varsMatrix(band, slotA) && varsMatrix(band, slotB)) ).toSeq
	
	val eachSlotUsedOnce: Seq[Formula] = (for{
		slot <- slots
		bandA <- bands
		bandB <- bands
	} yield !(varsMatrix(bandA, slot) && varsMatrix(bandB, slot)) ).toSeq
	
	

	 //combining all the constraints together
    val allConstraints: Seq[Formula] =
      desirableSlots ++ eachBandPlaysOnce ++ eachSlotUsedOnce
  
    //finding a satisfying assignment to the constraints
    val res = solveForSatisfiability(and(allConstraints:_*))
  
    res.map(model => {
      bands.map(band => {
        val assignedSlot = slots.find(slot => model(varsMatrix((band, slot))))
        (band, assignedSlot.get)
      }).toMap
    })
	 
  
	
	
	
	
   import Arithmetic._
 
 /**
   * This function takes a list of constraint, and returns a
   * constraint that is true if and only if at most max
   * of them are true.
   */
  def positivesLessEquals(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositives(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  }

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
  }
  
  val v1 = Volunteer("Marc")
  val v2 = Volunteer("Jean")
  val v3 = Volunteer("Dom")
  val v4 = Volunteer("Fabien")
  
  val t1 = Task("Bar", 1)
  val t2 = Task("Scene", 1)
  val t3 = Task("Lumieres", 1)
  val t4 = Task("Son", 2)
  
  val volunteers: List[Volunteer] = List(v1, v2)
	val tasks: List[Task] = List(t1,t2)
	val availability: Map[Volunteer, List[Task]] = Map(
		v1 -> List(t1), // Marc -> Bar
		v2 -> List(t2) // Jean -> Bar, Scene, Lumières
		//v3 -> List(t3, t4), // Dom -> Lumières, Son
		//v4 -> List(t3, t4) // Fabien -> Lumières, Son
	)
	val maxWorkload: Int = 2

   // generates one propositional variable per volunteer/task combination
    val varsMatrixTV: Map[(Volunteer, Task), PropVar] =
      volunteers.flatMap({ case v@Volunteer(name) =>
        tasks.map(t => (v, t) -> propVar(name))
      }).toMap
	
	val getVolunteersByTasks : Map[Task, List[Volunteer]] = tasks.map({
      case (task:Task) =>
        val listVol = availability.toList.filter { a => a._2.contains(task) }.map(x => x._1)
      (task, listVol)
   }).toMap
   

	val tasksByVolunteer : Map[Volunteer, List[PropVar]] =
  	availability.map {
  		case (v: Volunteer, tasksList: List[Task]) =>
  			(v, tasksList.map(varsMatrixTV(v, _)))
  	}
  	
  	val propsByTask: Map[Task, List[PropVar]] =
      tasks.map(t => t -> volunteers.map(v => varsMatrixTV(v, t))).toMap
      
   val propsByVolunteer: Map[Volunteer, List[PropVar]] =
      volunteers.map(v => v -> tasks.map(t => varsMatrixTV(v, t))).toMap
      
  val maxWorkloadRespected: Seq[Formula] =
      propsByVolunteer
        .map{ case (v, ts) => positivesLessEquals(ts, maxWorkload) }
        .toSeq
	
	// Ensure that all Volunteers are only assigned at desirable tasks (tasks matching in availability mapping)
	val desirableTasks: Seq[Formula] =
 	(tasksByVolunteer.toList.map{
 		case (v:Volunteer, propVarTasksList:List[PropVar]) =>
 			propVarTasksList.foldLeft[Formula](false)(_ || _)
 	}).toSeq
	 
	  val eachTaskCapacityFullyUsed : Seq[Formula] = (for {
      task <- tasks
      volunteer <- volunteers
  
      if(getVolunteersByTasks(task).contains(volunteer) && getVolunteersByTasks(task).size >= task.capacity)
    } yield varsMatrixTV(volunteer, task) ).toSeq
    
    
    //combining all the constraints together
    val allConstraintsTV: Seq[Formula] =
      desirableTasks ++ eachTaskCapacityFullyUsed //++ eachVolunteerAssignedInRespectWithWorkload
  
    //finding a satisfying assignment to the constraints
    val resTV = solveForSatisfiability(and(allConstraintsTV:_*))
    

    
    resTV.map(model => {
        tasks.map(task => {
          val assignedVolunteer = volunteers.filter(volunteer => model.contains(varsMatrixTV((volunteer, task))))
          (task, assignedVolunteer)
        }).toMap
      })
	 // Ensure that all volunteers are assigned to tasks in respect with the maximum Workload.

   val eachVolunteerAssignedInRespectWithWorkload : Seq[Formula] = availability.map{
   	case (v:Volunteer, tasksList:List[Task]) =>
   		if(tasksList.size<=maxWorkload) varsMatrixTV( (v, _) )
   }
   
   positivesLessEquals(
  
     def putToTheSameLength(n1: List[String], n2: List[String]): (List[String], List[String]) =
  ((for (x <- List.range(0, n2.length) if x >= n1.length) yield "") ::: n1, (for (x <- List.range(0, n1.length) if x >= n2.length) yield "") ::: n2)
   
   
   val n1 = List("A0", "A1" , "A2", "A3")
   val n2 = List("B0", "B1")
   
   val n = n1.reverse.zipAll(n2.reverse, "", "").unzip
   val (vn1, vn2) = (n._1.reverse, n._2.reverse)
   
   putToTheSameLength(n1,n2)
}