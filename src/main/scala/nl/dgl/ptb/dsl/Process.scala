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
    Future[Exchange](xnge)
  }

  override def process(xnge: Exchange): Exchange = {
    val xnge4this = xnge.step(this.index)

    xnge.listeners ++= xngeListeners
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    top.listeners += notifySubStepChanged
    val xngeFromTopStep = top.process(xnge4this)
    top.listeners -= notifySubStepChanged
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
    xnge.listeners --= xngeListeners

    return xngeFromTopStep
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

  override def process(xnge: Exchange): Exchange = ???

}

case class StepSequential private (val a: Step, val b: Step, i: Int) extends Step(i) {

  def this(a: Step, b: Step) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = {
    println("StepSequential.split: a=" + a + ",b=" + b)
    StepSequential(a.split, b.split, StepConstructionHelper.counter.getAndIncrement())
  }

  override def apply(xnge: Exchange): Future[Exchange] = {
    Future[Exchange](xnge)
  }

  override def process(xnge: Exchange): Exchange = {
    a.listeners += notifySubStepChanged
    val xngeFromA = a.process(xnge)
    a.listeners -= notifySubStepChanged
    b.listeners += notifySubStepChanged
    val xngeFromB = b.process(xngeFromA);
    b.listeners -= notifySubStepChanged
    return xngeFromB
  }

}

class StepChoice private (steps: Vector[Step], chooser: String, i: Int) extends Step(i) {

  def this(steps: Vector[Step], chooser: String) = this(steps, chooser, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def apply(xnge: Exchange) = ???

  override def process(xnge: Exchange): Exchange = ???

}

case class StepSplit private (splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step, i: Int) extends Step(i) {

  def this(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step) = this(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def apply(xnge: Exchange): Future[Exchange] = {
    Future[Exchange](xnge)
  }

  val items2StepPromise = Promise[Map[Any, Step]]()
  val items2StepFuture = items2StepPromise.future

  override def process(xnge: Exchange): Exchange = {
    val xnge4this = xnge.step(this.index);

    if (xnge4this.getIsStepFinished()) {
      println("StepSplit: isFinished! index=" + index)
      return xnge4this
    }

    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    val splitList = xnge.get[List[Any]](splitListKey)
    val items2Step = splitList.map {
      (_, stepToSplit.split())
    } toMap;
    items2StepPromise.success(items2Step)
    val splitResults = new HashMap[Any, Any]
    for (splitItem <- splitList) {
      val stepForItem = items2Step.get(splitItem).get
      val xnge4splitStep = xnge4this.step(stepForItem.index)
      xnge4splitStep.put(splitItemKey, splitItem)
      stepForItem.listeners += notifySubStepChanged
      val xnge4splitStepResult = stepForItem.process(xnge4splitStep);
      stepForItem.listeners -= notifySubStepChanged
      val splitItemResult = xnge4splitStepResult.get[Any](splitItemResultKey)
      splitResults.put(splitItem, splitItemResult)
    }
    xnge4this.put(splitResultsKey, splitResults.toMap)
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))

    xnge4this.setStepIsFinished()

    return xnge4this
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

    val xngePromise = Promise[Exchange]()
    val xngeFuture = xngePromise.future

    if (xnge4this.getIsStepFinished()) {
      println("Leap: isFinished! index=" + index)
      xngePromise.success(xnge)
    } else {
      listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
      xngePromise.completeWith(f(xnge4this))
      xngeFuture.andThen({
        case Success(xngeResult) => {
          xngeResult.setStepIsFinished()
        }
      })
    }
    xngeFuture.andThen({
      case Success(xngeResult) => {
        listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
      }
    })
    return xngeFuture
  }

  /**
   * Execute the action's function synchronously. Returns the exchange or throws the exception provided by the function's future.
   */
  @throws(classOf[Exception])
  def process(xnge: Exchange): Exchange = {
    val result = apply(xnge)
    Await.result(result, Duration.Inf)
    if (result.value.get.isSuccess) {
      return result.value.get.get
    } else {
      throw result.value.get.failed.get
    }
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
    }
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
  }

  override def apply(xnge: Exchange): Future[Exchange] = {
    applySync(xnge)
    val xngePromise = Promise[Exchange]()
    val xngeFuture = xngePromise.future
    xngePromise.success(xnge)
    Future[Exchange](xnge)
  }

  override def process(xnge: Exchange): Exchange = {
    applySync(xnge)
    return xnge;
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