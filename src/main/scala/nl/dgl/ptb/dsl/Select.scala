package nl.dgl.ptb.dsl

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import java.time.Instant

trait Selector[T] {
  def select(candidates: List[T], selectionPromise: Promise[T])
}

object DSL {

  val Selector = "Selector"
  val Selection = "Selection"
}

case class ActionSelect[T] private (filter: SelectFilter[T], i: Int)(implicit selector: Selector[T]) extends Action(i) {

  def this(filter: SelectFilter[T])(implicit selector: Selector[T]) = this(filter, StepConstructionHelper.counter.incrementAndGet())

  def split: Action = {
    println("StepSelect.split: NOT CLONED filter=" + filter)
    ActionSelect(filter, StepConstructionHelper.counter.incrementAndGet())
  }

  // selectionFuture because the selection will occur in the future
  val selectionPromise = Promise[T]()
  val selectionFuture = selectionPromise.future

  // candidatesPromise because we will know about the candidates when the xnge arrives.
  val candidatesPromise = Promise[List[T]]()
  val candidatesFuture = candidatesPromise.future

  override def f(xnge: Exchange): Future[Exchange] = {

    val candidates = filter.candidates(xnge)
    candidatesPromise.success(candidates)

    // ask the selector to select one of the candidates
    // note: the user can select as well via the UI
    selector.select(candidates, selectionPromise)

    val xngePromise = Promise[Exchange]()

    selectionFuture.andThen({ // may be completed by the selector or by user via the UI.
      case Success(selection) => {
        xnge.put(DSL.Selection, selection)
        xngePromise.success(xnge)
      }
      case Failure(cause) =>
        xngePromise.failure(cause)
    })

    return xngePromise.future;
  }

}

/**
 * Sets Select(ion) on Exchange
 */
object Select { // The UI presents the list to the user to select from, the selector does it some other way
  def apply[T](filter: SelectFilter[T])(implicit selector: Selector[T]) = {
    new ActionSelect(filter)
  }
}

trait SelectSource[T] { // Pallets object
  def Where(xngeKey: String): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}

trait SelectFilter[T] { // Palets instantce
  def And(xngeKey: String): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}
