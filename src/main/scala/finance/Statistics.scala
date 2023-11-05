package finance

import com.github.tototoshi.csv.CSVReader

import java.io.File
import scala.annotation.tailrec

object Statistics {

  def main(args: Array[String]): Unit = {
    val apple = quotazioni("AAPL.csv")
    val meta = quotazioni("META.csv")
    val google = quotazioni("GOOGL.csv")
    val amazon = quotazioni("AMZN.csv")
    val microsoft = quotazioni("MSFT.csv")
    val gM = quotazioni("GM.csv")

    //    println(correlation(List(9.0, 20.0, 30.0, 40.0, 48.0, 55.0, 61.0, 66.0), List(8.0, 20.0, 31.0, 42.0, 52.0, 61.0, 68.0, -14.0)))
    //
    println(variance(google))
    println(variance(amazon))
    println(variance(microsoft))
    println(variance(apple))

    System.out.println("---------------------------------------------------")
    println(correlation(gM, microsoft))
    //compoundInterestOverYears(1_000_000, 0.05, 10)
    println(correlation(google, google))
    println(correlation(meta, google))
    println(correlation(amazon, google))
    println(correlation(meta, meta))

    //    println(correlation(apple, google))
    //    println(standardDeviation(amazon))
    //    println(standardDeviation(google))
    //    println(standardDeviation(meta))
    //    println(standardDeviation(apple))
    //println(correlation(generalMotor, google))
    //println(correlation(generalMotor, fiat))
  }


  @tailrec
  private def compoundInterestOverYears(capital: Double, interestRate: Double, totalTime: Double, currentYears: Double = 0): Unit = {
    totalTime match {
      case 1 => {
        println(s"Year ${currentYears.toInt}: $capital")
      }
      case _ => {
        println(s"Year ${currentYears.toInt}: $capital")
        compoundInterestOverYears(capitalWithInterest(capital, interestRate), interestRate, totalTime - 1, currentYears + 1)
      }
    }
  }


  private def capitalWithInterest(capital: Double, interestRate: Double) = {
    Math.round(capital * (1 + interestRate))
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
    val av = average(list)
    list.foldLeft(0.0) { (acc, v) =>
      acc + (v - av) * (v - av)
    }   / (list.size - 1)
  }

  def average(list: List[Double]): Double = {
    val values = list.map(v => (v, 1 / list.size.toDouble))
    expectedValue(values)
  }

  def expectedValue(values: List[(Double, Double)]): Double = {
    values.map {
      case (a, b) => a * b
    }.sum
  }

  def cov(a: List[Double], b: List[Double]): Double = {
    val avA = average(a)
    val avB = average(b)
    a.zip(b).map {
      case (v1, v2) => {
        println((v1 - avA) * (v2 - avB))
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
