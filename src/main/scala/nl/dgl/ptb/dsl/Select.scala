package nl.dgl.ptb.dsl

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap

trait Selector[T] {
  def select(candidates: List[T], selectionPromise: Promise[T]) // should drop promise if it it completed.
  def getSelectType(): Class[T]
  def getSelectLocation(): Location
  Selector.put(getSelectLocation(), getSelectType(), this)
}

object Selector {

  def apply[T](selectLocation: Location, selectType: Class[T]): Selector[T] = {
    location2selectors.get(selectLocation).get(selectType).asInstanceOf[Selector[T]]
  }

  private val location2selectors = HashMap.empty[Location, HashMap[Class[_], Selector[_]]]

  def put[T](selectLocation: Location, selectType: Class[T], selector: Selector[T]) = {
    location2selectors.getOrElseUpdate(selectLocation, HashMap.empty[Class[_], Selector[_]]).put(selectType, selector)
  }

}

object DSL {

  val Location = "Location"
  val Selection = "Selection"
}

case class ActionSelect[T] private (filter: SelectFilter[T], i: Int) extends Action(i) {

  def this(filter: SelectFilter[T]) = this(filter, StepConstructionHelper.counter.incrementAndGet())

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

    val selectType = filter.getSelectType()
    val selectLocation = xnge.get[Location](DSL.Location)

    val selector = Selector(selectLocation, selectType)

    // ask the selector to select one of the candidates
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
  def apply[T](filter: SelectFilter[T]) = {
    new ActionSelect(filter)
  }
}

trait SelectSource[T] {
  def Where(xngeKey: String): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}

trait SelectFilter[T] {
  def And(xngeKey: String): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
  def getSelectType(): Class[T]
}
