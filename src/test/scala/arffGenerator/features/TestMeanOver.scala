package arffGenerator.features

/**
 * User: Vincent DOBA
 * Date: 10/18/11
 * Time: 6:32 PM
 */
import org.scalatest.Suite
import arffGenerator.data.Data

class TestMeanOver extends Suite {

  def testFindLocalMaximum() {
    val data:Data = new Data("data.csv")
    data.prepareData()

    expect(2613) {MeanOver.findLocalMaximum(data)._2.toInt}
    expect(10) {MeanOver.findLocalMaximum(data)._1}

  }


}