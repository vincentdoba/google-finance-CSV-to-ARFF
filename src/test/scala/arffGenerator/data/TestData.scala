package arffGenerator.data

/**
 * User: Vincent DOBA
 * Date: 09/10/11
 * Time: 23:03
 */

import org.scalatest.Suite

class TestData() extends Suite {

  /**
   * Test if the function getAttribute of Data class is correctly performed
   */
  def testGetAttribute() {
    val data:Data = new Data("data.csv")
    val values:List[Double] = data.getAttribute("close")

    // verifying the size of the list
    expect(1523) {values.size}
    // verifying if the first element is correctly loaded
    expect(2699) {values.head.toInt}
    // verifying if the last element is correctly loaded
    expect(2600) {values.last.toInt}
  }

  /**
   * Test if the function mean5 of Data class is correctly performed
   */
  def testMean5() {
    val data:Data = new Data("data.csv")
    data.mean5()

    // verifying the size of the data
    expect(1523) {data.samples.size}

    // verifying that the two first values (prolongated values) are correctly computed
    expect (2683) {data.samples(0)("mean5").toInt}
    expect (2651) {data.samples(1)("mean5").toInt}

    // verifying that the first value that have been computed using simple mean is correct
    expect(2623) {data.samples(2)("mean5").toInt}

    expect(2602) {data.samples(8)("mean5").toInt}

    // verifying that the last value is correctly computed
    expect (2600) {data.samples.last("mean5").toInt}
  }

  /**
   * Test if the data is correctly loaded
   */
  def testCorrectLoading() {
    val data:Data = new Data("data.csv")

    // verifying the size of the data
    expect(1523) {data.samples.size}

    // verifying that the first element is correctly loaded
    expect (2699.toDouble) {data.samples(0)("close")}
    expect (2703.toDouble) {data.samples(0)("high")}
    expect (2673.toDouble) {data.samples(0)("open")}
    expect (2631.toDouble) {data.samples(0)("low")}
    expect (67809210.toDouble) {data.samples(0)("volume")}

    // verifying that the last element is correctly loaded
    expect (2600.toDouble) {data.samples.last("close")}
    expect (2627.toDouble) {data.samples.last("high")}
    expect (2609.toDouble) {data.samples.last("open")}
    expect (2586.toDouble) {data.samples.last("low")}
    expect (61084200.toDouble) {data.samples.last("volume")}
  }

  /**
   * Test if method sinus of class Data is correctly performed
   */
  def testSinus() {
    val data:Data = new Data("data.csv")
    data.sinus()

    // verifying the size of the data
    expect(1523) {data.samples.size}
    // verifying that the first sinus is correctly computed
    expect (440015) {(data.samples(0)("sinus")*1000000).toInt}
    // verifying that the last sinus is correctly computed
    expect (-157990) {(data.samples.last("sinus")*1000000).toInt}
  }

  def testExtrema() {
    val data:Data = new Data("data.csv")
    data.mean5()
    data.extrema()

    // verifying the size of the data
    expect(1523) {data.samples.size}
    // verifying the first element
    expect(0) {data.samples.head("extrema").toInt}
    expect(0) {data.samples(1)("extrema").toInt}
    expect(0) {data.samples(2)("extrema").toInt}
    expect(0) {data.samples(3)("extrema").toInt}
    // verifying the first extrema
    expect(-2585) {data.samples(6)("extrema").toInt}
    // verifying the second extrema
    expect(2613) {data.samples(10)("extrema").toInt}
    // verifying the third extrema
    expect(-2444) {data.samples(16)("extrema").toInt}
    // verifying the last extrema
    expect(-2534) {data.samples(1518)("extrema").toInt}
  }

  def testSinus5() {
    val data:Data = new Data("data.csv")
    data.sinus5()

    // verifying the size of the data
    expect (1523) {data.samples.size}
    // verifying the first value
    expect (-780868) {(data.samples.head("sinus5")*1000000).toInt}
  }

  def testRemoveFirstSample() {
    val data:Data = new Data("data.csv")
    data.prepareData()
    data.removeFirstSample()

    // verifying the size of the data
    expect(1522) {data.samples.size}
    // verifying the last value
    expect (2600.toDouble) {data.samples.last("close")}
    expect (2627.toDouble) {data.samples.last("high")}
    expect (2609.toDouble) {data.samples.last("open")}
    expect (2586.toDouble) {data.samples.last("low")}
    expect (61084200.toDouble) {data.samples.last("volume")}
    expect(-2534) {data.samples(1517)("extrema").toInt}
    expect (-157990) {(data.samples.last("sinus")*1000000).toInt}
    expect (2600) {data.samples.last("mean5").toInt}
    // verifying the first value
    expect (2650) {data.samples.head("close").toInt}
    expect (2680) {data.samples.head("high").toInt}
    expect (2617) {data.samples.head("open").toInt}
    expect (2589) {data.samples.head("low").toInt}
    expect (66742534) {data.samples.head("volume").toInt}
    expect (2632) {data.samples.head("mean5").toInt}
    expect (417905) {(data.samples.head("sinus")*1000000).toInt}

    expect(-2585) {data.samples(5)("extrema").toInt}
    // verifying the second extrema
    expect(2613) {data.samples(9)("extrema").toInt}
    // verifying the third extrema
    expect(-2444) {data.samples(15)("extrema").toInt}
  }

}