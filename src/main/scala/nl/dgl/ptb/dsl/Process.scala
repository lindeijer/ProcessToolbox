package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap

class Process(val top: Step) extends Step {

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

  def notifySubStepChanged(stepEvent: StepEvent) = {
    listeners.foreach(_.apply(stepEvent))
  }

  val xngeListeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

}

object Process {

  def apply(top: Step) = {
    new Process(top)
  }

}

// ------

case class StepConcurrent(a: Step, b: Step) extends Step {

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

  def notifySubStepChanged(subStepEvent: StepEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

case class StepSequential(val a: Step, val b: Step) extends Step {

  override def step(xnge: Exchange) = {}

  override def process(xnge: Exchange) = {
    a.listeners += notifySubStepChanged
    a.process(xnge)
    a.listeners -= notifySubStepChanged
    b.listeners += notifySubStepChanged
    b.process(xnge);
    b.listeners -= notifySubStepChanged
  }

  def notifySubStepChanged(subStepEvent: StepEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

class StepChoice(steps: Vector[Step], chooser: String) extends Step {

  override def step(xnge: Exchange) = {
    val chosenIndex = xnge.get[Int](chooser)
    val chosenStep = steps(chosenIndex)
    chosenStep.process(xnge)
  }

}

class StepSplit(splitListKey: Any, splitItemKey: Any, splitItemResultKey: Any, splitResultsKey: Any, step: Step) extends Step {

  override def step(xnge: Exchange) = {
    val splitList = xnge.get[List[Any]](splitListKey)
    println("StepSplit: " + splitList + "=" + splitList);
    val splitResults = new HashMap[Any, Any]
    splitList.foreach(splitItem => {
      xnge.put(splitItemKey, splitItem)
      step.process(xnge);
      val splitItemResult = xnge.get[Any](splitItemResultKey)
      splitResults.put(splitItem, splitItemResult)
    })
    xnge.put(splitResultsKey, splitResults.toMap)
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

abstract class Step {

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

}

class StepFunction(val f: Exchange => Any) extends Step {

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
