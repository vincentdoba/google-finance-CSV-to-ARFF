package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 6:37 PM
 */

object Consecutive extends Numeric {

  override def name:String = "consecutive"

  override def compute(data:Data):String = {
    val result = computeNumeric(data)
    if (result==0) "?" else result.toString
  }


  def computeNumeric(data:Data):Int = {
    def consecutive(listValue:List[Double], consecutiveDay:Int, lastValue:Double):Int = listValue match {
      case Nil => consecutiveDay
      case a::b if (a > lastValue && consecutiveDay>0) => consecutiveDay
      case a::b if (a < lastValue && consecutiveDay<0) => consecutiveDay
      case a::b if (a > lastValue && consecutiveDay==0) => consecutive(b, -1, a)
      case a::b if (a > lastValue && consecutiveDay<0) => consecutive(b, consecutiveDay-1, a)
      case a::b if (a < lastValue && consecutiveDay==0) => consecutive(b, 1, a)
      case a::b if (a < lastValue && consecutiveDay>0) => consecutive(b, consecutiveDay+1, a)
      case a::b if (a==lastValue) => consecutive(b, consecutiveDay, a)
      case _ => consecutiveDay
    }

    consecutive(data.getAttribute("close").tail, 0, data.samples.head("close"))
  }

}