package nl.dgl.ptb.dsl

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._

class Select(filter: SelectFilter[_]) extends Step {

  def step(xnge: Exchange): Unit = {
    val candidates = filter.candidates(xnge); // the last filter
    val selection = Future {
      candidates.head
    }
    Try(Await.ready(selection, Duration.Inf)) match { // anti-pattern, in future change to onSuccess
      case Success(selectedCandidate) => { xnge.put(Select, selectedCandidate) }
      case Failure(_)                 => { println("Failure Happened") }
      case _                          => { println("Very Strange") }
    }
  }
}

object Select { // IU presents the list to select from, selector does it some other way
  def apply(filter: SelectFilter[_]) = {
    new Select(filter)
  }
}

trait SelectSource[T] { // Pallets object
  def Where(xngeKey: Any): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}

trait SelectFilter[T] { // Palets instantce
  def And(xngeKey: Any): SelectFilter[T]
  def candidates(xnge: Exchange): List[T]
}
