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

case class StepSelect private (filter: SelectFilter[_], i: Int) extends Step(i) {

  def this(filter: SelectFilter[_]) = this(filter, StepConstructionHelper.counter.incrementAndGet())

  def split: Step = {
    println("StepSelect.split: NOT CLONED filter=" + filter)
    StepSelect(filter, StepConstructionHelper.counter.incrementAndGet())
  }

  // selectionFuture because the selection will occur in the future
  val selectionPromise = Promise[Any]()
  val selectionFuture = selectionPromise.future

  // candidatesPromise because we will know about the candidates when the xnge arrives.
  val candidatesPromise = Promise[List[Any]]()
  val candidatesFuture = candidatesPromise.future

  def step(xnge: Exchange): Unit = {
    val candidates = filter.candidates(xnge)
    candidatesPromise.success(candidates)
    def notifySelection(selection: Any) = {
      if (candidates.contains(selection)) {
        selectionPromise.success(selection)
      }
    }
    val selector = xnge.get[Selector](Selector)
    selector.listeners += notifySelection

    // start listening to the selector. The UI may select as well.

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
