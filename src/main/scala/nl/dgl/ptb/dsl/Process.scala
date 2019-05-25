package nl.dgl.ptb.dsl

import java.util.HashMap
import scala.collection.mutable.ListBuffer
import java.time.Instant

class ProcessEvent {}
case class ProcessExchangeChanged(process: Process, xngeEvent: ExchangeEvent) extends ProcessEvent {}
case class ProcessStepChanged(process: Process, xngeEvent: StepEvent) extends ProcessEvent {}

class Process(val top: Step) {

  def process(xnge: Exchange) = {
    xnge.listeners += notifyExchangeChanged
    top.listeners += notifyStepChanged
    top.process(xnge)
    top.listeners -= notifyStepChanged
    xnge.listeners -= notifyExchangeChanged
  }

  ////////////////////////

  def notifyExchangeChanged(xngeEvent: ExchangeEvent) = {
    listeners.foreach(_.apply(ProcessExchangeChanged(this, xngeEvent)))
  }

  def notifyStepChanged(stepEvent: StepEvent) = {
    listeners.foreach(_.apply(ProcessStepChanged(this, stepEvent)))
  }

  val listeners: ListBuffer[ProcessEvent => Unit] = ListBuffer.empty
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
    val chosenIndex = xnge.get(chooser).asInstanceOf[Int]
    val chosenStep = steps(chosenIndex)
    chosenStep.process(xnge)
  }

}

class StepSplit(in: String, out: String, step: Step) extends Step {

  override def step(xnge: Exchange) = {
    val inputList = xnge.get(in).asInstanceOf[List[Any]]
    val outputMap = new HashMap[Any, Any]
    inputList.foreach(input => {
      xnge.put(in, input)
      step.process(xnge);
      val output = xnge.get(out)
      outputMap.put(input, output)
    })
    xnge.put(out, outputMap)
  }

}

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

class StepFunction(f: Exchange => Any) extends Step {

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
    properties.get(key).asInstanceOf[T];
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
    if (properties.containsKey(key)) {
      listeners.foreach(_.apply(ExchangeRemove(key)))
      properties.remove(key);
    }
  }

  def containsKey(key: Any) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.containsKey(key);
  }

  val listeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

  // ----

  private val stash = new HashMap[Any, Any]

  def stash_get(key: Any) = {
    stash.get(key);
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
    stash.containsKey(key);
  }

}
