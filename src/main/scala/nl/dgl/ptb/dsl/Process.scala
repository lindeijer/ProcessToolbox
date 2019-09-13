package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger
import gremlin.scala.label
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

case class Process private (val top: Step, i: Int) extends Step(i) {

  def this(top: Step) = this(top, StepConstructionHelper.counter.getAndIncrement())

  override def split(): Step = {
    println("Process.split: top=" + top)
    Process(top.split())
  }

  override def apply(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index)
    xnge.listeners ++= xngeListeners
    val promiseXngeTop = Promise[Exchange]();
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    top.listeners += notifySubStepChanged
    top.apply(xnge4this).andThen({
      case Success(xngeFromTopStep) => {
        top.listeners -= notifySubStepChanged
        listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
        xnge.listeners --= xngeListeners
        promiseXngeTop.success(xngeFromTopStep)
      }
    })
    promiseXngeTop.future
  }

  val xngeListeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

  override def toString(): String = {
    return "Process@" + i;
  }

}

object Process {

  def apply(top: Step) = {
    new Process(top)
  }

}

// ------

case class StepConcurrent private (a: Step, b: Step, i: Int) extends Step(i) {

  def this(a: Step, b: Step) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def apply(xnge: Exchange): Future[Exchange] = {
    Future[Exchange](xnge)
  }

}

case class StepSequential private (val a: Step, val b: Step, i: Int) extends Step(i) {

  def this(a: Step, b: Step) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = {
    println("StepSequential.split: a=" + a + ",b=" + b)
    StepSequential(a.split, b.split, StepConstructionHelper.counter.getAndIncrement())
  }

  override def apply(xnge: Exchange): Future[Exchange] = {
    a.listeners += notifySubStepChanged
    val promiseXngeB = Promise[Exchange]();
    a.apply(xnge).andThen({
      case Success(xngeFromA) => {
        a.listeners -= notifySubStepChanged
        b.listeners += notifySubStepChanged
        b.apply(xngeFromA).andThen({
          case Success(xngeFromB) => {
            b.listeners -= notifySubStepChanged
            promiseXngeB.success(xngeFromB)
          }
        });
      }
    })
    promiseXngeB.future
  }

}

class StepChoice private (steps: Vector[Step], chooser: String, i: Int) extends Step(i) {

  def this(steps: Vector[Step], chooser: String) = this(steps, chooser, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def apply(xnge: Exchange) = ???

}

//wow, check out https://github.com/S-Mach/s_mach.concurrent

import scala.collection.mutable.ListBuffer

case class StepSplit private (splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step, i: Int) extends Step(i) {

  def this(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step) = this(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  val promiseSteps = Promise[List[Step]]()
  val futureSteps = promiseSteps.future // the view looks at this.

  override def apply(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index);
    if (xnge4this.getIsStepFinished()) {
      println("StepSplit: isFinished! index=" + index)
      Future[Exchange](xnge)
    }
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    val items = xnge.get[List[Any]](splitListKey)
    val items2steps = items.map(item => {
      val stepForItem = stepToSplit.split()
      (item, stepForItem)
    })
    val steps = items2steps.map(item2step => {
      item2step._2
    })
    promiseSteps.success(steps)
    val futuresForItems = items2steps.map(item2step => {
      val item = item2step._1
      val stepForItem = item2step._2
      val xngeForItem = xnge4this.step(stepForItem.index)
      xngeForItem.put(splitItemKey, item)
      stepForItem.listeners += notifySubStepChanged
      stepForItem.apply(xngeForItem).andThen({
        case _ => {
          stepForItem.listeners -= notifySubStepChanged
        }
      })
    })
    val splitResults = new HashMap[Any, Any]
    Future.sequence(futuresForItems).andThen({ // with the list of exchanges from the split steps.
      case Success(xnges) => {
        xnges.foreach(xnge => {
          val item = xnge.get[Any](splitItemKey)
          val resultForItem = xnge.get[Any](splitItemResultKey)
          splitResults.put(item, resultForItem)
        })
        xnge4this.put(splitResultsKey, splitResults.toMap)
      }
      case Failure(reason) => {
        xnge4this.put(splitResultsKey, reason)
      }
    }).andThen({ // finally
      case _ => {
        listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
        xnge4this.setStepIsFinished()
      }
    }).map(_ => { // return the split's exchange rather that the list of split exchanges.
      xnge4this
    })
  }

}

object Split {

  def apply(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, step: Step): StepSplit = {
    return new StepSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, step)
  }

  def apply(splitItemKey: String, step: Step): StepSplit = {
    return new StepSplit(splitItemKey + "List", splitItemKey, splitItemKey + "Result", splitItemKey + "ResultList", step)
  }

}

// ----

trait StepEvent {
  val step: Step
  val instant: Instant
}
case class StepStarted(step: Step, instant: Instant) extends StepEvent {}
case class StepFinished(step: Step, instant: Instant) extends StepEvent {}

object StepConstructionHelper {
  val counter = new AtomicInteger(0)
}

/**
 * Action, actually. Everything is an action.
 */
@label("step")
abstract class Step(val index: Int) {

  def split(): Step

  def ~>(next: Step): Step = {
    new StepSequential(this, next);
  }

  def &&(next: Step): StepConcurrent = {
    new StepConcurrent(this, next)
  }

  def f(xnge: Exchange): Future[Exchange] = ???

  /**
   * The action's asynchronous function.
   */
  def apply(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index)
    if (xnge4this.getIsStepFinished()) {
      println("Leap: isFinished! index=" + index)
      listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
      return Future[Exchange](xnge4this);
    }
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    val xngeFutureResult = f(xnge4this)
    xngeFutureResult.andThen({
      case Success(xngeResult) => {
        xngeResult.setStepIsFinished()
        listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
      }
    })
    return xngeFutureResult
  }

  val listeners: ListBuffer[StepEvent => Unit] = ListBuffer.empty

  def notifySubStepChanged(subStepEvent: StepEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

/**
 * Step Action that executes locally.
 */
case class StepSync private (val f: Exchange => Any, i: Int) extends Step(i) {

  def this(f: Exchange => Any) = this(f, StepConstructionHelper.counter.getAndIncrement())

  def split(): Step = {
    StepSync(f, StepConstructionHelper.counter.getAndIncrement()) // NO CLONE HERE OF F, SO THE F MAY NOT BE STATEFULL
  }

  private def applySync(xnge: Exchange) = {
    val xnge4this = xnge.step(this.index)
    if (xnge4this.getIsStepFinished()) {
      println("Step: isFinished! index=" + index)
    } else {
      listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
      f.apply(xnge) // the xnge is modified as a side effect.
      xnge4this.setStepIsFinished()
    }
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
  }

  override def apply(xnge: Exchange): Future[Exchange] = {
    applySync(xnge)
    Future[Exchange](xnge)
  }

}

object Step extends StepSync(null) {

  def apply(f: Exchange => Any): Step = {
    return new StepSync(f);
  }

}

/////////////////////////////////

case class StepAsync private (val function: Exchange => Future[Exchange], i: Int) extends Step(i) {

  def this(f: Exchange => Future[Exchange]) = this(f, StepConstructionHelper.counter.getAndIncrement())

  def split(): Step = {
    StepAsync(f, StepConstructionHelper.counter.getAndIncrement()) // NO CLONE HERE OF F, SO THE F MAY NOT BE STATEFULL
  }

  override def f(xnge: Exchange): Future[Exchange] = function.apply(xnge)

}

object Leap extends StepAsync(null) {

  def apply(f: Exchange => Future[Exchange]): Step = {
    return new StepAsync(f);
  }

}