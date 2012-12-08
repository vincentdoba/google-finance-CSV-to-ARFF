package arffGenerator.features

/**
 * User: Vincent DOBA
 * Date: 9/28/11
 * Time: 2:25 PM
 */

trait Mean {

  def mean(values:List[Int]):Int = {
    def meanCompute(values:List[Int], sum:Int, size:Int):Int = values match {
      case Nil => sum/size
      case elem::l => meanCompute(l, elem+sum, size+1)
    }

    meanCompute(values, 0, 0)
  }

}