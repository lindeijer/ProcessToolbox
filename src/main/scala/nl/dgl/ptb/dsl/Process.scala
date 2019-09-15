package nl.dgl.ptb.dsl

import java.time.Instant

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success

import gremlin.scala.label

case class Process private (val top: Action, i: Int) extends Action(i) {

  def this(top: Action) = this(top, StepConstructionHelper.counter.getAndIncrement())

  override def split(): Action = {
    println("Process.split: top=" + top)
    Process(top.split())
  }

  override def start(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index)
    xnge.listeners ++= xngeListeners
    val promiseXngeTop = Promise[Exchange]();
    listeners.foreach(_.apply(new ActionStarted(this, Instant.now)))
    top.listeners += notifySubStepChanged
    top.start(xnge4this).andThen({
      case Success(xngeFromTopStep) => {
        top.listeners -= notifySubStepChanged
        listeners.foreach(_.apply(new ActionFinished(this, Instant.now)))
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

  def apply(top: Action) = {
    new Process(top)
  }

}

// ------

case class ActionConcurrent private (a: Action, b: Action, i: Int) extends Action(i) {

  def this(a: Action, b: Action) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Action = ???

  override def start(xnge: Exchange): Future[Exchange] = {
    Future[Exchange](xnge)
  }

}

case class ActionSequential private (val a: Action, val b: Action, i: Int) extends Action(i) {

  def this(a: Action, b: Action) = this(a, b, StepConstructionHelper.counter.getAndIncrement)

  def split(): Action = {
    println("StepSequential.split: a=" + a + ",b=" + b)
    ActionSequential(a.split, b.split, StepConstructionHelper.counter.getAndIncrement())
  }

  override def start(xnge: Exchange): Future[Exchange] = {
    a.listeners += notifySubStepChanged
    val promiseXngeB = Promise[Exchange]();
    a.start(xnge).andThen({
      case Success(xngeFromA) => {
        a.listeners -= notifySubStepChanged
        b.listeners += notifySubStepChanged
        b.start(xngeFromA).andThen({
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

class ActionChoice private (steps: Vector[Action], chooser: String, i: Int) extends Action(i) {

  def this(steps: Vector[Action], chooser: String) = this(steps, chooser, StepConstructionHelper.counter.getAndIncrement)

  def split(): Action = ???

  override def start(xnge: Exchange) = ???

}

//wow, check out https://github.com/S-Mach/s_mach.concurrent

case class ActionSplit private (splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Action, i: Int) extends Action(i) {

  def this(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, stepToSplit: Action) = this(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, StepConstructionHelper.counter.getAndIncrement)

  def split(): Action = ???

  val promiseSteps = Promise[List[Action]]()
  val futureSteps = promiseSteps.future // the view looks at this.

  override def start(xnge: Exchange): Future[Exchange] = {
    val xnge4this = xnge.step(this.index);
    if (xnge4this.getIsStepFinished()) {
      println("StepSplit: isFinished! index=" + index)
      Future[Exchange](xnge)
    }
    listeners.foreach(_.apply(new ActionStarted(this, Instant.now)))
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
      stepForItem.start(xngeForItem).andThen({
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
        listeners.foreach(_.apply(new ActionFinished(this, Instant.now)))
        xnge4this.setStepIsFinished()
      }
    }).map(_ => { // return the split's exchange rather that the list of split exchanges.
      xnge4this
    })
  }

}

object Split {

  def apply(splitListKey: String, splitItemKey: String, splitItemResultKey: String, splitResultsKey: String, step: Action): ActionSplit = {
    return new ActionSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, step)
  }

  def apply(splitItemKey: String, step: Action): ActionSplit = {
    return new ActionSplit(splitItemKey + "List", splitItemKey, splitItemKey + "Result", splitItemKey + "ResultList", step)
  }

}
