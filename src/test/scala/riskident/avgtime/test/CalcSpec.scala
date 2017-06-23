package riskident.avgtime.test

import org.scalatest._
import riskident.avgtime.{Calc, Request}

import scala.util.Try

class CalcSpec extends FlatSpec with Matchers {
  "The Calc object" should "match the test examples" in {
    locally {
      val requests = List(
        Request(0, 0, 3),
        Request(1, 1, 9),
        Request(2, 2, 6)
      )
      val bestAvgtime = Calc(requests)
      bestAvgtime shouldEqual 9

      requests.permutations.map {
        requests =>
          (Calc.totalWait(requests).toDouble / 3.0).toInt
      }.min shouldEqual bestAvgtime
    }

    locally {
      val requests = List(
        Request(0, 0, 3),
        Request(1, 1, 9),
        Request(2, 2, 5)
      )
      val bestAvgtime = Calc(requests)
      bestAvgtime shouldEqual 8

      requests.permutations.map {
        requests =>
          (Calc.totalWait(requests).toDouble / 3.0).toInt
      }.min shouldEqual bestAvgtime
    }

    locally {
      val requests = List(
        Request(0, 0, 3),
        Request(1, 1, 9),
        Request(2, 2, 1),
        Request(3, 2, 7),
        Request(4, 3, 8),
        Request(5, 4, 1)
      )
      val minTotalWait = Calc.minTotalWait(requests)
      requests.permutations.map {
        requests =>
          Try(Calc.totalWait(requests)).getOrElse(Int.MaxValue)
      }.min shouldEqual minTotalWait
    }

  }
}
