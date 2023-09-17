package finance

import com.github.tototoshi.csv.CSVReader

import java.io.File

object Statistica {

  def main(args: Array[String]): Unit = {
    val apple = quotazioni("AAPL.csv")
    val meta = quotazioni("META.csv")
    val google = quotazioni("GOOGL.csv")
    val amazon = quotazioni("AMZN.csv")
    val microsoft = quotazioni("MSFT.csv")
    val gM = quotazioni("GM.csv")


    println(correlation(List(1500, 1700, 1400, 1600), List( 200, 350, 150, 300)))
    println(average(List(1500, 1700, 1400, 1600)))
    println(cov(List(1500, 1700, 1400, 1600), List( 200, 350, 150, 300)))


//    println(correlation(List(9.0, 20.0, 30.0, 40.0, 48.0, 55.0, 61.0, 66.0), List(8.0, 20.0, 31.0, 42.0, 52.0, 61.0, 68.0, -14.0)))
//
    println(correlation(gM, microsoft))
//    println(correlation(meta, google))
//    println(correlation(amazon, google))
//    println(correlation(apple, google))
//    println(standardDeviation(amazon))
//    println(standardDeviation(google))
//    println(standardDeviation(meta))
//    println(standardDeviation(apple))
    //println(correlation(generalMotor, google))
    //println(correlation(generalMotor, fiat))
  }


  private def quotazioni(stock: String) = {
    CSVReader.open(new File(stock)).allWithHeaders().map(line => line("Adj Close").toDouble)
  }

  def standardDeviation(a: List[Double]): Double = {
    val avA = average(a)

    Math.sqrt({
      (a.map(v => (v - avA) * (v - avA)).sum) / (a.size - 1)
    })
  }

  def variance(list: List[Double]): Double = {
    (list.map(v => (v - average(list)) * (v - average(list))).sum) / (list.size - 1)
  }

  def average(list: List[Double]): Double = {
    val values = list.map(v => (v, 1 / list.size.toDouble))
    expectedValue(values)
  }

  def expectedValue(values: List[(Double,Double)]): Double = {
    values.map {
      case (a, b) => a * b
    }.sum
  }

  def cov(a: List[Double], b: List[Double]): Double ={
    val avA = average(a)
    val avB = average(b)
    a.zip(b).map{
      case (v1, v2) => {
        println( (v1 - avA) * (v2 - avB))
        (v1 - avA) * (v2 - avB)
      }
    }.sum / (a.size)
  }

  def correlation(a: List[Double], b: List[Double]) = {
    val avA = average(a)
    val avB = average(b)
    val cov = a.zip(b).map {
      case (v1, v2) => (v1 - avA) * (v2 - avB)
    }.sum / (a.size)

    val devStdA = Math.sqrt({
      (a.map(v => (v - avA) * (v - avA)).sum) / (a.size)
    })

    val devStdB = Math.sqrt({
      (b.map(v => (v - avB) * (v - avB)).sum) / (b.size)
    })

    cov / (devStdA * devStdB)
  }

}
