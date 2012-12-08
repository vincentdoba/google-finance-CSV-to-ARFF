package arffGenerator.features

import arffGenerator.data.Data

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 4:18 PM
 */

abstract class Numeric extends Feature {
  override def nature:String = "NUMERIC"
}