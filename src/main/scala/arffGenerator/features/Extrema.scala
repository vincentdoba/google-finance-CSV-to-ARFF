package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/28/11
 * Time: 4:06 PM
 */

class Max(val length:Int) extends Numeric {
  override def name:String = "max" + length.toString
  // TODO implement method
  override def compute(data:Data):String = ""
  // TODO implement method
  override def computeList(data:Data):List[String] = Nil
}

class Min(val length:Int) extends Numeric {
  override def name:String = "min" + length.toString

  // TODO implement method
  override def compute(data:Data):String = ""
  // TODO implement method
  override def computeList(data:Data):List[String] = Nil
}