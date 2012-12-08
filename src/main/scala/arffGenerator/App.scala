package arffGenerator

import features._

/**
 * @author ${user.name}
 */
object App {
  
  def main(args : Array[String]) {
    val features:Array[Feature] = Array(MeanOver, Consecutive, ChangeVariation)
    val arffCreator:ArffCreator = new ArffCreator("data.csv", features)
    arffCreator.createArffFile()
  }

}
