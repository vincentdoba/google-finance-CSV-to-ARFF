package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 6:19 PM
 */

class MeanVariation(val length:Int) extends Numeric with Mean {
  override def name:String = "meanVariation" + length.toString
  // TODO implement method
  override def compute(data:Data):String = ""
  // TODO implement method
  override def computeList(data:Data):List[String] = Nil

}