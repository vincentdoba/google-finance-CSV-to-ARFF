package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 2:10 PM
 */

/**
 * Feature aims to represent a Feature, meaning a object that, given a data, compute a value
 * over these data that represents a characteristic of these data
 */
trait Feature {

  // Name of the feature, that will be used to retrieve it and to name it from arff files
  def name:String
  // Nature of the feature, used for arff file
  def nature:String

  /**
   * given data, compute the value of the feature for the first line of the data samples
   * and return it as a string
   *
   * @param the data you want to compute the feature of the first line
   * @return the value of the feature as a String
   */
  def compute(data:Data):String = "?"

  /**
   * given data, compute the value of the feature for each line of the data samples and
   * return it as a list of String
   *
   * @param the data you want to compute feature
   * @return the list of feature values as a String
   */
  def computeList(data:Data):List[String] = {
    def compute(data:Data, result:List[String]):List[String] = data match {
      case value if (value.samples == Nil) => result.reverse
      case value => {
        val buffer = this.compute(value)
        value.removeFirstSample()
        compute(value, buffer::result)
      }
    }

    compute(data.copyData(), Nil)
  }

}