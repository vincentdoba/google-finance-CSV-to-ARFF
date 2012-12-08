package arffGenerator.features

import arffGenerator.data.Data
import scala.math.abs

/**
 * User: Vincent DOBA
 * Date: 10/18/11
 * Time: 5:35 PM
 */

object MeanOver extends Numeric {

  override def name:String = "MeanOver"

  override def compute(data:Data) = computeNumeric(data).toString

  def computeNumeric(data:Data):Double = {
    val maximum = MeanOver.findLocalMaximum(data)
    val value = (data.samples(0)("close"), data.samples(0)("mean5"))
    abs((value._1 - value._2)/(0.0001 + (data.samples(maximum._1)("close") - maximum._2)))
  }

  /**
   * determine which maximum in the past is the most close from the current considered day
   * It returns the position of the maximum and its value
   *
   * @param data the data you want to know the closest maximum of the first sample
   * @return a 2-tuple giving the position and the value of the maximum
   */
  def findLocalMaximum(data:Data):(Int, Double) = {
    def compute(values:List[Double], position:Int):(Int, Double) = values match {
      case Nil => (0, 0.0)
      case elem1::l if (elem1>0) => (position, elem1)
      case elem1::l => compute(l, position+1)
    }

    compute(data.getAttribute("extrema"), 0)
  }

}