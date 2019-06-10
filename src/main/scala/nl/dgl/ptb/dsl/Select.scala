package nl.dgl.ptb.dsl

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._

case class StepSelect(filter: SelectFilter[_]) extends Step { // NOT STATELESS, CAN'T BE REUSED IN SPLIT

  // selectionFuture because the selection will occur in the future
  var selectionPromise = Promise[Any]()
  var selectionFuture = selectionPromise.future

  // candidatesPromise because we will know about the candidates when the xnge arrives.
  var candidatesPromise = Promise[List[Any]]()
  var candidatesFuture = candidatesPromise.future

  def step(xnge: Exchange): Unit = {
    if (candidatesPromise.isCompleted) {
      println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      selectionPromise = Promise[Any]()
      selectionFuture = selectionPromise.future
      candidatesPromise = Promise[List[Any]]()
      candidatesFuture = candidatesPromise.future
    }
    candidatesPromise.success(filter.candidates(xnge))
    Try(Await.ready(selectionFuture, Duration.Inf)) match { // anti-pattern, in future change to onSuccess
      case Success(selectedCandidate) => { xnge.put(Select, selectedCandidate) }
      case Failure(_)                 => { println("Failure Happened") }
      case _                          => { println("Very Strange") }
    }
  }
}

object Select { // IU presents the list to select from, selector does it some other way
  def apply(filter: SelectFilter[_]) = {
    new StepSelect(filter)
  }
}

trait SelectSource[T] { // Pallets object
  def Where(xngeKey: Any): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}

trait SelectFilter[T] { // Palets instantce
  def And(xngeKey: Any): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}
