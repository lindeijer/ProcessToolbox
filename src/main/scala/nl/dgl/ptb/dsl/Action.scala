package nl.dgl.ptb.dsl

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

import gremlin.scala.label

trait ActionEvent {
  val step: Action
  val instant: Instant
}
case class ActionStarted(step: Action, instant: Instant) extends ActionEvent {}
case class ActionFinished(step: Action, instant: Instant) extends ActionEvent {}

object StepConstructionHelper {
  val counter = new AtomicInteger(0)
}

@label("step")
abstract class Action(val index: Int) {

  def split(): Action

  def ~>(next: Action): Action = {
    new StepSequential(this, next);
  }

  def &&(next: Action): StepConcurrent = {
    new StepConcurrent(this, next)
  }

  def f(xnge: Exchange): Future[Exchange] = ???

  /**
   * The action's asynchronous function.
   */
  def start(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index)
    if (xnge4this.getIsStepFinished()) {
      println("Leap: isFinished! index=" + index)
      listeners.foreach(_.apply(new ActionFinished(this, Instant.now)))
      return Future[Exchange](xnge4this);
    }
    listeners.foreach(_.apply(new ActionStarted(this, Instant.now)))
    val xngeFutureResult = f(xnge4this)
    xngeFutureResult.andThen({
      case Success(xngeResult) => {
        xngeResult.setStepIsFinished()
        listeners.foreach(_.apply(new ActionFinished(this, Instant.now)))
      }
    })
    return xngeFutureResult
  }

  val listeners: ListBuffer[ActionEvent => Unit] = ListBuffer.empty

  def notifySubStepChanged(subStepEvent: ActionEvent) = {
    listeners.foreach(_.apply(subStepEvent))
  }

}

case class BasicAction private (val f: Exchange => Future[Exchange], i: Int) extends Action(i) {

  def this(f: Exchange => Future[Exchange]) = this(f, StepConstructionHelper.counter.getAndIncrement())

  def split(): Action = {
    BasicAction(f, StepConstructionHelper.counter.getAndIncrement()) // NO CLONE HERE OF F, SO THE F MAY NOT BE STATEFULL
  }

}

object Action extends BasicAction(null) {

  def apply(f: Exchange => Future[Exchange]): Action = {
    return new BasicAction(f);
  }

}

/////////////////////////////////////////

/**
 * Step Action that executes synchronously, which means that future returned by start is complete.
 */
case class Step private (val f: Exchange => Any, i: Int) extends Action(i) {

  def this(f: Exchange => Any) = this(f, StepConstructionHelper.counter.getAndIncrement())

  def split(): Action = {
    Step(f, StepConstructionHelper.counter.getAndIncrement()) // NO CLONE HERE OF F, SO THE F MAY NOT BE STATEFULL
  }

  private def applySync(xnge: Exchange) = {
    val xnge4this = xnge.step(this.index)
    if (xnge4this.getIsStepFinished()) {
      println("Step: isFinished! index=" + index)
    } else {
      listeners.foreach(_.apply(new ActionStarted(this, Instant.now)))
      f.apply(xnge) // the xnge is modified as a side effect.
      xnge4this.setStepIsFinished()
    }
    listeners.foreach(_.apply(new ActionFinished(this, Instant.now)))
  }

  override def start(xnge: Exchange): Future[Exchange] = {
    applySync(xnge)
    Future[Exchange](xnge)
  }

}

object Step extends Step(null) {

  def apply(f: Exchange => Any): Action = {
    return new Step(f);
  }

}

