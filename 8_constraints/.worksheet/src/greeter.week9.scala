package greeter

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

  type Slot = (Stage, Time);import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(402); 
  
  val s1 = Stage("Scene 1");System.out.println("""s1  : greeter.week9.Stage = """ + $show(s1 ));$skip(28); 
  val s2 = Stage("Scene 2");System.out.println("""s2  : greeter.week9.Stage = """ + $show(s2 ));$skip(352); 
  
  // def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] =
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
  );System.out.println("""preferences  : Map[greeter.week9.Band,List[greeter.week9.Slot]] = """ + $show(preferences ));$skip(114); 

  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] =
    preferences.toList.flatMap(_._2).toSet;System.out.println("""getUniqueSlots: (preferences: Map[greeter.week9.Band,List[greeter.week9.Slot]])Set[greeter.week9.Slot]""");$skip(45); 
   
  val desirableSlots: Seq[Formula] = ???;System.out.println("""desirableSlots  : Seq[cafesat.api.Formulas.Formula] = """ + $show(desirableSlots ))}
  

}
