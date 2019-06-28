package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
 * This component implements a constraint solver for assigning time slots to volunteers
 * for various tasks at a festival. A task may require more than one volunteer,
 * and a volunteer can take a limited number of tasks
 */
object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
   * A task is represented by its name and
   * its capacity, i.e. the exact number of people
   * required to complete it.
   */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
   * This function schedules volunteers to tasks.
   * It takes as input a list of volunteers and a list of tasks.
   * The `availability` map contains mappings from volunteers to the
   * tasks they are available for.
   * A volunteer can be assigned to several tasks, but only
   * up to a maximum number of task specified by the `maxWorkload` parameter.
   * It is ok to not assign a volunteer to any task.
   *
   * The return value is a list of volunteers assigned to each task. The function only
   * returns a complete valid assignment, if no such assignment exists then the
   * function returns None.
   */
  def schedule(
    volunteers: List[Volunteer],
    tasks: List[Task],
    availability: Map[Volunteer, List[Task]],
    maxWorkload: Int
  ): Option[Map[Task, List[Volunteer]]] = {

    // generates one propositional variable per volunteer/task combination
    val varsMatrixTV: Map[(Volunteer, Task), PropVar] =
      volunteers.flatMap({ case v@Volunteer(name) =>
        tasks.map(t => (v, t) -> propVar(name))
      }).toMap  

      
   val propsByVolunteer: Map[Volunteer, List[PropVar]] =
      volunteers.map(v => v -> tasks.map(t => varsMatrixTV(v, t))).toMap
   
    // Ensure that all Volunteers are only assigned at desirable tasks (tasks matching in availability mapping)   
    val desirableTasks : Seq[Formula] =
      (availability.toList.map{
        case (v:Volunteer, tasksList:List[Task]) =>
          tasksList.foldLeft[Formula](false)(_ || varsMatrixTV(v, _))
      }).toSeq 
      
    val propsByTask: Map[Task, List[PropVar]] =
      tasks.map(t => t -> volunteers.map(v => varsMatrixTV(v, t))).toMap
      
    // Ensure that all tasks have the minimum amount of volunteers assigned to them  
    val capacityReached: Seq[Formula] =
      propsByTask.toSeq
        .map{ case (t, vs) => t -> countPositives(vs) }
        .flatMap{ case (t, (c, cc)) => cc + lessEquals(int2binary(t.capacity), c) }

    val maxWorkloadRespected: Seq[Formula] =
        propsByVolunteer.map{ case (v, ts) => positivesLessEquals(ts, maxWorkload) }.toSeq
    
    //combining all the constraints together
    val allConstraints: Seq[Formula] = 
      desirableTasks ++ capacityReached ++ maxWorkloadRespected
  
    //finding a satisfying assignment to the constraints
    val res = solveForSatisfiability(and(allConstraints:_*))

    res.map(model => {
      tasks.map(task => {
        val assignedVolunteer = volunteers.filter(volunteer => model(varsMatrixTV((volunteer, task))))
        (task, assignedVolunteer)
      }).toMap
    })
    
  }

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

}
