package arffGenerator.data

import io.Source
import scala.collection.mutable.Map
import scala.math._

/**
 * User: Vincent DOBA
 * Date: 9/27/11
 * Time: 2:20 PM
 */

/**
 * Stock data sample
 */
class Data(var samples:List[Map[String, Double]]) {
  /**
   * Create data given a file
   *
   * @param pathToFile the path to the file
   */
  def this(pathToFile:String) = this(Data.createSamples(pathToFile))

  /**
   * Compute all the parameters on data
   */
  def prepareData() {
    sinus()
    sinus5()
    mean5()
    extrema()
  }

  /**
   * return the first sample of the data, and update all the other computed values
   */
  def removeFirstSample():Map[String, Double] = {
    val result = samples.head
    // compute the new values
    this.samples = this.samples.tail
    this.mean5()
    this.extrema()

    result
  }

  /**
   * Given a list of parameter values and the name of the parameter, append it to the sample list
   *
   * @param parameterValues the list of values
   * @param parameterName the name of the parameter
   * @throw RuntimeException if the parameter values list is not of the same size than the samples list
   */
  def addParameter(parameterValues:List[Double], parameterName:String)  {
    def compute(values:List[Map[String, Double]], result:List[Map[String, Double]], parameterValues:List[Double]):List[Map[String, Double]] = {
      (values, parameterValues) match {
        case (Nil, Nil) => result.reverse
        case (elem1::l1, elem2::l2) => {
          elem1 += (parameterName -> elem2)
          compute(l1, elem1::result, l2)
        }
        case _ => throw new RuntimeException("values list and " + parameterName + " list should have the same size")
      }
    }

    samples = compute(samples, Nil, parameterValues)
  }

  /**
   * given the name of an attribute, return the list of the attribute
   *
   * @Param the name of the attribute you want to extract
   */
  def getAttribute(attributeName:String):List[Double] = {
    // getAttribute with terminal recursion
    def compute(values:List[Map[String, Double]], result:List[Double]):List[Double] = values match {
      case Nil => result.reverse
      case elem1::l => compute(l, elem1(attributeName)::result)
    }

    compute(samples, Nil)
  }

  /**
   * Compute the sinus of the angle ABC, where A is (0,0), B is (1,0) and
   * C is (0, close1 - close2)
   */
  def computeSinus(close1:Double, close2:Double):Double = (close1 - close2) / sqrt(10000 + pow(close1 - close2, 2))

  /**
   * Add the list of sinus of angle ABC, where A is (0,0), B is (1,0)
   * and C is (0,closeValueOfYesterday - closeValueOfToday)
   */
  def sinus() {

    def compute(values:List[Map[String, Double]],result:List[Map[String, Double]]):List[Map[String, Double]] =
    values match {
      case Nil => result.reverse
        case elem1::Nil => {
          // for the last element, as there is not previous one, we take the last computed sinus as sinus
          elem1 += ("sinus" -> result.head("sinus"))
          (elem1::result).reverse
        }
        case elem1::elem2::l => {
          elem1 += ("sinus" -> computeSinus(elem1("close"), elem2("close")))
          compute(elem2::l, elem1::result)
        }
    }

    this.samples = compute(this.samples, Nil)
  }

  def sinus5() {
    def compute(values:List[Map[String, Double]], result:List[Double], buffer:List[Double]):List[Double] =
    values match {
      case Nil if (!buffer.isEmpty) => compute(Nil, result.head::result, buffer.tail)
      case Nil => result.reverse
      case elem1::l if (buffer.size<4) => compute(l, result, elem1("close")::buffer)
      case elem1::l => compute(l, computeSinus(elem1("close"), buffer.last)::result, elem1("close")::buffer.dropRight(1))
    }

    this.addParameter(compute(samples, Nil, Nil), "sinus5")
  }

  /**
   * Add the list of mean over the four neighboring values and the considered value
   * After performing this method, you can ask "mean5" over the Maps contained in samples
   */
  def mean5() {
    // do the mean of a list of values
    def mean(values:List[Double]):Double = {
      def compute(values:List[Double], sum:Double, size:Double):Double = values match {
        case Nil => sum/size
        case elem1::l => compute(l, sum+elem1, size+1)
      }
      compute(values, 0, 0)
    }

    // compute the values that are at the edge of the list
    def extension(values:List[Double], variation:Double):Double = {
      def compute(values:List[Double], size:Int, sum:Double, variation:Double):Double = values match {
        case Nil => sum
        case elem1::Nil => sum + (5 - size)*elem1 + (1.3*(5-size-1) - 0.3)*variation
        case elem1::l => compute(l, size+1, sum + elem1, variation)
      }

      (compute(values.reverse, 0, 0, variation))/5
    }

    // compute the list of means, given the list of Map  (function for terminal recursion)
    def compute(values:List[Map[String, Double]], result:List[Double], buffer:List[Double]):List[Double] =
    values match {
        case Nil if (buffer.size==5) => compute(Nil, mean(buffer)::result, buffer.dropRight(1))
        case Nil if (buffer.size>2) => {
          // for the left end of the tab, we use a prolongation function
          compute(Nil, (extension(buffer, result(0) - result(1)))::result, buffer.dropRight(1))
        }
        case Nil => result
        case elem1::l if (buffer.size < 2) => {
          // for the right end of the table, we put the value as mean
          compute(l, elem1("close")::result, elem1("close")::buffer)
        }
        case elem1::l if (buffer.size < 5) => compute(l, result, elem1("close")::buffer)
        case elem1::l => compute(l, mean(buffer)::result, elem1("close")::buffer.dropRight(1))
      }

    // compute the list of means, given the list of Map
    def meanList(values:List[Map[String, Double]]):List[Double] = compute(values, Nil, Nil)

    // add means to the Maps of samples
    this.addParameter(meanList(samples.reverse), "mean5")
  }

  /**
   * Determine the list of extrema
   */
  def extrema() {
    val values:List[Double] = this.getAttribute("mean5")

    // find the extrema position, return the list determining wether a cotation is an extrema
    // 0 if the index cotation is not an extrema, 1 if it is a maximum and -1 if it is a minimum
    def compute(values:List[Double], result:List[Int], lastComputed:Int):List[Int] = values match {
      case Nil => result.reverse
      case elem1::Nil => (0::result).reverse
      case elem1::elem2::l if ((elem1-elem2)>0 && lastComputed>=0)=> compute(elem2::l, 0::result, 1)
      case elem1::elem2::l if ((elem1-elem2)>0 && lastComputed==(-1)) => compute(elem2::l, -1::result, 1)
      case elem1::elem2::l if ((elem1-elem2)<0 && lastComputed==1) => compute(elem2::l, 1::result, -1)
      case elem1::elem2::l if ((elem1-elem2)<0 && lastComputed<=0) => compute(elem2::l, 0::result, -1)
      case elem1::elem2::l if ((elem1-elem2)==0.0) => compute(elem2::l, 0::result, lastComputed)
    }

    // Thanks to the list of extrema, save the extrema in the data
    // the value saved is the one contained in mean5 index
    def addExtrema(values:List[Map[String, Double]], result:List[Map[String, Double]], extremas:List[Int]):List[Map[String, Double]] = {
      (values, extremas) match {
        case (Nil,Nil) => result.reverse
        case (elem1::l1, elem2::l2) => {
          elem1 += ("extrema" -> -elem2*elem1("mean5"))
          addExtrema(l1, elem1::result, l2)
        }
        case _ => throw new RuntimeException("values list and extremas list should have the same size")
      }
    }

    samples = addExtrema(samples, Nil, compute(values, Nil, 0))
  }

  def copyData():Data = {

    def copySamples(samples:List[Map[String, Double]], result:List[Map[String, Double]]):List[Map[String, Double]] =
      samples match {
        case Nil => result.reverse
        case elem1::l => copySamples(l, elem1::result)
      }

    new Data(copySamples(this.samples, Nil))
  }
}

object Data {

  /**
   * Transform a String to an array of int contained in the String
   *
   * The line input format is as follow :
   *   15-Sep-11,26.73,27.03,26.31,26.99,67809210
   * And the result is
   *   Array[Int] = Array(2673,2703,2631,2699,67809210)
   */
  def treatLine(line:String):Array[Double] = line.replace(".", "").split(",").toList.tail.map(x => Integer.parseInt(x).toDouble).toArray
  def readFile(file:String): List[String] = Source.fromFile(file).getLines().toList.tail
  def getLines(file:String):List[Array[Double]] = for {line <- readFile(file)} yield treatLine(line)
  def createSamples(file:String):List[Map[String, Double]] = for {line <- getLines(file)} yield createMap(line)

  def createMap(values:Array[Double]):Map[String, Double] = {
    var map = Map[String, Double]()
    map += ("open" -> values(0))
    map += ("high" -> values(1))
    map += ("low" -> values(2))
    map += ("close" -> values(3))
    map += ("volume" -> values(4))
    map
  }

}