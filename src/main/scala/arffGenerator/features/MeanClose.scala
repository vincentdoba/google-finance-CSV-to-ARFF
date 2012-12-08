package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 5:03 PM
 */

class MeanClose(val length:Int) extends Numeric with Mean {

  override def name:String = "meanClose" + length.toString

  // TODO implement method
  override def compute(data:Data):String = ""
  // TODO implement method
  override def computeList(data:Data):List[String] = Nil

}