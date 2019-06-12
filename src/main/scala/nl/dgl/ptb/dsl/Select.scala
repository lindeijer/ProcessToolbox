package nl.dgl.ptb.dsl

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer

trait Selector {
  val listeners: ListBuffer[Any => Unit] = ListBuffer.empty
}

object Selector
object Selection

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

    def notifySelection(selection: Any) = {
      selectionPromise.success(selection)
    }

    val selector = xnge.get[Selector](Selector)
    selector.listeners += notifySelection

    // start listening to the selector. THe UI mat select as well.

    candidatesPromise.success(filter.candidates(xnge))
    Try(Await.ready(selectionFuture, Duration.Inf)) match { // anti-pattern, in future change to onSuccess
      case Success(selectedCandidate) => { xnge.put(Selection, selectedCandidate.value.get.get) }
      case Failure(_)                 => { println("Failure Happened") }
      case _                          => { println("Very Strange") }
    }
    selector.listeners -= notifySelection
  }
}

/**
 * Sets Select(ion) on Exchange
 */
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
