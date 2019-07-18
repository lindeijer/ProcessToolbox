package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger
import gremlin.scala.label

case class Process private (val top: Step, i: Int) extends Step(i) {

  def this(top: Step) = this(top, StepConstructionHelper.counter.getAndIncrement())

  override def split(): Step = {
    println("Process.split: top=" + top)
    Process(top.split())
  }

  override def step(xnge: Exchange) = {}

  override def process(xnge: Exchange): Exchange = {

    val xnge4this = xnge.step(this.index)

    if (xnge4this.getIsStepFinished()) {
      println("Process: isFinished! index=" + index)
      return xnge4this
    }

    xnge.listeners ++= xngeListeners
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    top.listeners += notifySubStepChanged
    val xngeFromTopStep = top.process(xnge4this)
    top.listeners -= notifySubStepChanged
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))

    xnge4this.setStepIsFinished()

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

  override def step(xnge: Exchange) = {}

  override def process(xnge: Exchange): Exchange = ???

}

case class StepSequential private (val a: Step, val b: Step, i: Int) extends Step(i) {

  def this(a: Step, b: Step) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = {
    println("StepSequential.split: a=" + a + ",b=" + b)
    StepSequential(a.split, b.split, StepConstructionHelper.counter.getAndIncrement())
  }

  override def step(xnge: Exchange) = {}

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

  override def step(xnge: Exchange) = ???

}

case class StepSplit private (splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step, i: Int) extends Step(i) {

  def this(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Step) = this(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def step(xnge: Exchange) = {}

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

@label("step")
abstract class Step(val index: Int) {

  def split(): Step

  def ~>(next: Step): Step = {
    new StepSequential(this, next);
  }

  def &&(next: Step): StepConcurrent = {
    new StepConcurrent(this, next)
  }

  /**
   * Execute the step's function. User extensions of this class must override this method.
   */
  def step(xnge: Exchange)

  /**
   * Execute the step. User extensions of this class should not override this method.
   */
  def process(xnge: Exchange): Exchange = { 
    val xnge4this = xnge.step(this.index)

    if (xnge4this.getIsStepFinished()) {
      println("Step: isFinished! index=" + index)
      return xnge4this
    }

    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    step(xnge4this) // the xnge is modified as a side effect.
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))

    xnge4this.setStepIsFinished();

    return xnge4this;
  }

  val listeners: ListBuffer[StepEvent => Unit] = ListBuffer.empty

  def notifySubStepChanged(subStepEvent: StepEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

case class StepFunction private (val f: Exchange => Any, i: Int) extends Step(i) {

  def this(f: Exchange => Any) = this(f, StepConstructionHelper.counter.getAndIncrement())

  def split(): Step = {
    StepFunction(f, StepConstructionHelper.counter.getAndIncrement()) // NO CLONE HERE OF F, SO THE F MAY NOT HAVE SIDE-EFFECTS
  }

  override def step(xnge: Exchange) = {
    f.apply(xnge)
  }

}

object Step extends StepFunction(null) {

  def apply(f: Exchange => Any): Step = {
    return new StepFunction(f);
  }

}

