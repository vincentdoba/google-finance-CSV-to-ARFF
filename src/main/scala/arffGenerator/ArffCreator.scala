package arffGenerator

/**
 * User: Vincent DOBA
 * Date: 25/09/11
 * Time: 17:10
 */

import data.Data
import features.Feature
import java.io.FileWriter

class ArffCreator(val file:String, val features:Array[Feature]) {
  val data = new Data(file)
  var buffer = this.initBuffer()

  def initBuffer():List[String] = {
    def compute(values:List[scala.collection.mutable.Map[String, Double]], result:List[String]):List[String] =
      values match {
        case Nil => result
        case elem1::l => compute(l, ""::result)
    }

    compute(data.samples, Nil)
  }

  def createArffFile() {
    val fw = new FileWriter("result.arff")
    data.prepareData()
    fw.write("@RELATION stock\n\n")

    for (feature <- features) {
      this.addFeature(feature)
      fw.write("@ATTRIBUTE " + feature.name + " " + feature.nature + "\n")
    }

    fw.write("\n")
    fw.write("@DATA\n")

    for (line <- this.buffer) {
      fw.write(line + "\n")
    }
    fw.close()
  }


  def addDataValue(nameValue:String) {
    addValues(this.data.getAttribute(nameValue))
  }

  def addFeature(feature:Feature) {
    addValues(feature.computeList(this.data))
  }

  def addValues[A](values:List[A]) {
    def compute[A](values:List[A], buffer:List[String], result:List[String]):List[String] = (values, buffer) match {
      case (Nil, Nil) => result.reverse
      case (elem1::l1, elem2::l2) if (elem2=="") => compute(l1, l2, elem1.toString::result)
      case (elem1::l1, elem2::l2) => compute(l1, l2, (elem2 + "," + elem1.toString)::result)
      case _ => throw new RuntimeException("values list and buffer list should have the same size")
    }

    this.buffer = compute(values, this.buffer, Nil)
  }

}