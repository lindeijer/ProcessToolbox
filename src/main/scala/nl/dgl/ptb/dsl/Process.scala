package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger

case class Process private (val top: Step, i: Int) extends Step(i) {

  def this(top: Step) = this(top, StepConstructionHelper.counter.incrementAndGet())

  override def split(): Step = {
    println("Process.split: top=" + top)
    Process(top.split())
  }

  override def step(xnge: Exchange) = {}

  override def process(xnge: Exchange) = {
    xnge.listeners ++= xngeListeners
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    top.listeners += notifySubStepChanged
    top.process(xnge)
    top.listeners -= notifySubStepChanged
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
    xnge.listeners --= xngeListeners
  }

  val xngeListeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

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

  override def process(xnge: Exchange) = {
    // concurrent implies any interleaving allowed, including sequential which is what we do for now.
    a.listeners += notifySubStepChanged
    a.process(xnge)
    a.listeners -= notifySubStepChanged
    b.listeners += notifySubStepChanged
    b.process(xnge);
    b.listeners -= notifySubStepChanged
  }

}

case class StepSequential private (val a: Step, val b: Step, i: Int) extends Step(i) {

  def this(a: Step, b: Step) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = {
    println("StepSequential.split: a=" + a + ",b=" + b)
    StepSequential(a.split, b.split, StepConstructionHelper.counter.incrementAndGet())
  }

  override def step(xnge: Exchange) = {}

  override def process(xnge: Exchange) = {
    a.listeners += notifySubStepChanged
    a.process(xnge)
    a.listeners -= notifySubStepChanged
    b.listeners += notifySubStepChanged
    b.process(xnge);
    b.listeners -= notifySubStepChanged
  }

}

class StepChoice private (steps: Vector[Step], chooser: String, i: Int) extends Step(i) {

  def this(steps: Vector[Step], chooser: String) = this(steps, chooser, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def step(xnge: Exchange) = {
    val chosenIndex = xnge.get[Int](chooser)
    val chosenStep = steps(chosenIndex)
    chosenStep.process(xnge)
  }

}

case class StepSplit private (splitListKey: Any, splitItemKey: Any, splitItemResultKey: Any, splitResultsKey: Any, stepToSplit: Step, i: Int) extends Step(i) {

  def this(splitListKey: Any, splitItemKey: Any, splitItemResultKey: Any, splitResultsKey: Any, stepToSplit: Step) = this(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, StepConstructionHelper.counter.getAndIncrement)

  def split(): Step = ???

  override def step(xnge: Exchange) = {}

  val items2StepPromise = Promise[Map[Any, Step]]()
  val items2StepFuture = items2StepPromise.future

  override def process(xnge: Exchange) = {
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    val splitList = xnge.get[List[Any]](splitListKey)
    val items2Step = splitList.map {
      (_, stepToSplit.split())
    } toMap;
    items2StepPromise.success(items2Step)
    val splitResults = new HashMap[Any, Any]
    for (splitItem <- splitList) {
      val stepForItem = items2Step.get(splitItem).get
      xnge.put(splitItemKey, splitItem)
      stepForItem.listeners += notifySubStepChanged
      stepForItem.process(xnge);
      stepForItem.listeners -= notifySubStepChanged
      val splitItemResult = xnge.get[Any](splitItemResultKey)
      splitResults.put(splitItem, splitItemResult)
    }
    xnge.put(splitResultsKey, splitResults.toMap)
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
  }

}

object Split {

  def apply(splitListKey: Any, splitItemKey: Any, splitItemResultKey: Any, splitResultsKey: Any, step: Step): StepSplit = {
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
  def process(xnge: Exchange) = {
    listeners.foreach(_.apply(new StepStarted(this, Instant.now)))
    step(xnge)
    listeners.foreach(_.apply(new StepFinished(this, Instant.now)))
  }

  val listeners: ListBuffer[StepEvent => Unit] = ListBuffer.empty

  def notifySubStepChanged(subStepEvent: StepEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

case class StepFunction private (val f: Exchange => Any, i: Int) extends Step(i) {

  def this(f: Exchange => Any) = this(f, StepConstructionHelper.counter.incrementAndGet())

  def split(): Step = {
    StepFunction(f, StepConstructionHelper.counter.incrementAndGet()) // NO CLONE HERE OF F, SO THE F MAY NOT HAVE SIDE-EFFECTS
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

class ExchangeEvent {}
case class ExchangePut(key: Any, value: Any) extends ExchangeEvent {}
case class ExchangeRemove(key: Any) extends ExchangeEvent {}
case class ExchangeRename(oldKey: Any, newKey: Any) extends ExchangeEvent {}

class Exchange {

  private val properties = new HashMap[Any, Any]

  def get[T](key: Any): T = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.get(key).getOrElse(null).asInstanceOf[T];
  }

  def rename(oldKey: Any, newKey: Any) = {
    if (oldKey == null) {
      if (newKey == null) {
        throw new IllegalArgumentException("Neither old-key nor new-key may be null, and both are null.");
      } else {
        throw new IllegalArgumentException("The old-key may not be null. Attempted with new-key=" + newKey);
      }
    }
    if (newKey == null) {
      throw new IllegalArgumentException("The new-key not be null. Allempted with old-key=" + oldKey);
    }
    listeners.foreach(_.apply(ExchangeRename(oldKey, newKey)))
    properties.put(newKey, properties.get(oldKey));
    properties.remove(oldKey)
  }

  def put(key: Any, value: Any) = {
    if (key == null) {
      if (value == null) {
        throw new IllegalArgumentException("Neither value nor key may be null, and both are null.");
      } else {
        throw new IllegalArgumentException("The key may not be null. Allempted use with value=" + value);
      }
    }
    if (value == null) {
      throw new IllegalArgumentException("The value may not be null. key=" + key);
    }
    listeners.foreach(_.apply(ExchangePut(key, value)))
    properties.put(key, value);
  }

  def remove(key: Any) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    if (properties.contains(key)) {
      listeners.foreach(_.apply(ExchangeRemove(key)))
      properties.remove(key);
    }
  }

  def containsKey(key: Any) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.contains(key);
  }

  val listeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

  // ----

  private val stash = new HashMap[Any, Any]

  def stash_get[T](key: Any): T = {
    if (key == null) {
      throw new IllegalArgumentException("The stash-key may not be null.");
    }
    stash.get(key).asInstanceOf[T];
  }

  def stash_rename(oldKey: Any, newKey: Any) = {
    stash.put(newKey, properties.get(oldKey));
    stash.remove(oldKey)
  }

  def stash_put(key: Any, value: Any) = {
    stash.put(key, value);
  }

  def stash_remove(key: Any) = {
    stash.remove(key);
  }

  def stash_containsKey(key: Any) = {
    stash.contains(key);
  }

}
