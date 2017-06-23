package riskident.avgtime

/**
  * Created by caeus on 22/06/17.
  */

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

//I added the id field, so that two customers with the same data won't create conflict
case class Request(id: Int, arrivalTime: Int, pizzaDelay: Int)

object Calc {
  //This is for testing purposes too, not necessary
  @tailrec
  def bestSolution(pendingRequests: List[Request], currentTime: Int = 0,
                   reverseSolution: List[Request] = Nil): List[Request] = {
    pendingRequests match {
      case Nil =>
        reverseSolution.reverse
      case _ =>
        val (waitingCustomers, futureCustomers) = pendingRequests.partition {
          request =>
            request.arrivalTime <= currentTime
        }
        waitingCustomers match {
          case Nil =>
            bestSolution(pendingRequests = pendingRequests,
              currentTime = currentTime + 1,
              reverseSolution = reverseSolution)
          case _ =>
            //Here I chose the one who affects less the waits of the others customers and the selected customer
            val next :: rest = waitingCustomers.sortBy {
              request =>
                waitingCustomers.map {
                  waitingRequest =>
                    currentTime - waitingRequest.arrivalTime + request.pizzaDelay
                }.sum
            }

            //The one I chose is removed from the list of waitingCustomers and is passed to the next iteration
            // with the future customers
            bestSolution(rest ::: futureCustomers, currentTime + next.pizzaDelay,
              next :: reverseSolution)
        }
    }
  }

  @tailrec
  def minTotalWait(pendingRequests: List[Request], currentTime: Int = 0,
                   accumulatedWait: Int = 0): Int = {
    pendingRequests match {
      case Nil =>
        accumulatedWait
      case _ =>
        //I only consider customers which have already arrived, because well Tieu doesn't know the future, does he?
        val (waitingCustomers, futureCustomers) = pendingRequests.partition {
          request =>
            request.arrivalTime <= currentTime
        }
        waitingCustomers match {
          case Nil =>
            //If for some reason (and this should never happen) I'm considering a point in time
            // Where there's no waiting customers, I go forward a step and continue
            minTotalWait(pendingRequests = pendingRequests,
              currentTime = currentTime + 1,
              accumulatedWait = accumulatedWait)
          case _ =>
            //Here I choose the one who affects less the waits of the others customers and the selected customer
            // Something more efficient that a sorting could be done here, maybe, but it probably includes using
            // mutable stuff and I prefer not to
            val next :: rest = waitingCustomers.sortBy {
              request =>
                waitingCustomers.map {
                  waitingRequest =>
                    currentTime - waitingRequest.arrivalTime + request.pizzaDelay
                }.sum
            }
            //The one I chose is removed from the list of waitingCustomers and they are passed to the next iteration
            // with the future customers
            minTotalWait(pendingRequests = rest ::: futureCustomers,
              currentTime = currentTime + next.pizzaDelay,
              accumulatedWait = accumulatedWait + currentTime - next.arrivalTime + next.pizzaDelay)
        }
    }
  }

  //I used this to test the whole permutations and check if my algorithm was right,
  // although it's not used in the real solution
  // only for testing purposes
  @tailrec
  def totalWait(solution: List[Request], accumulatedWait: Int = 0,
                waitOffset: Int = 0): Int = {
    solution match {
      case Nil => accumulatedWait
      case next :: rest =>
        val newWaitOffset = waitOffset + next.pizzaDelay
        val myWaitTime = newWaitOffset - next.arrivalTime
        if (myWaitTime < 0)
          throw new Exception("This solution is invalid")
        else
          totalWait(rest, accumulatedWait + myWaitTime, newWaitOffset)
    }
  }

  def apply(requests: List[Request]): Int = {
    (minTotalWait(requests).toDouble / requests.length.toDouble).toInt
  }

  def main(args: Array[String]): Unit = {
    Try(StdIn.readLine().toInt)
      .flatMap {
        quantity: Int =>
          Try {
            (0 until quantity).map {
              id =>
                val List(arrivalTime, pizzaDelay) = StdIn.readLine().split("\\s+").toList.map(_.toInt)
                Request(id, arrivalTime = arrivalTime, pizzaDelay = pizzaDelay)
            }.toList
          }
      }
      .map(apply) match {
      case Success(result) => println(result)
      case Failure(e) =>
        println("You probably entered something wrongly formatted")
        e.printStackTrace()
    }
  }
}