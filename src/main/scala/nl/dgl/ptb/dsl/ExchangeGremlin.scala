package nl.dgl.ptb.dsl

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.io.IoCore.gryo
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.__
import org.apache.tinkerpop.gremlin.process.traversal.{ Order, P, Scope }
import java.lang.{ Long ⇒ JLong, Double ⇒ JDouble }
import java.util.{ Map ⇒ JMap }
import scala.collection.JavaConversions._
import org.scalatest.{ Matchers, WordSpec }
import shapeless.HNil

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger

object XXX {

  val g: ScalaGraph = {
    val graph = TinkerGraph.open
    graph.io(gryo()).readGraph("movie-lens.kryo")
    graph.asScala
  }

  println("g=" + XXX.g);
  println("g.V.count.head=" + XXX.g.V.count.head);
  println("g.E.count.head=" + XXX.g.E.count.head);

  val person = Key[String]("person");
  val name = Key[String]("name");
  val weight = Key[String]("weight");

  //val v1 = XXX.g.addV("person").property(name, "marko") // .next()
  //val v2 = XXX.g.addV("person").property(name, "stephen") // .next()
  //val e1 = XXX.g.V(v1).addE("knows").to(v2) // .property(weight, 0.75) // .iterate()

}

import gremlin.scala._
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.apache.tinkerpop.gremlin.structure.Direction

@label("my_custom_label")
case class Example(longValue: Long, stringValue: Option[String])
// case class Person(name: String, friends: Seq[String])

@label("keyandvalue")
case class KeyAndValue(key: Option[Any], value: Option[Any])

object Main extends App {
  implicit val graph = TinkerGraph.open.asScala
  val example = Example(Long.MaxValue, Some("optional value"))
  val v = graph + example
  v.toCC[Example] // equal to `example`, but with `vertex` set

  // find all vertices with the label of the case class `Example`
  graph.V.hasLabel[Example]

  // modify the vertex like a case class
  v.updateAs[Example](_.copy(longValue = 0L))
}

object ExchangeGremlin {

  val graph = TinkerGraph.open.asScala
}

class ExchangeGremlin(step: Step, predecessor: Exchange) extends Exchange {

  def this() = this(new StepFunction(xnge => xnge), new ExchangeHashMap())

  override def step(step: Step): Exchange = {
    return new ExchangeGremlin(step, this);
  }

  import gremlin.scala._
  import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph

  implicit val graph = ExchangeGremlin.graph;

  val index = Key[Int]("index")
  val key = Key[Any]("key")

  val vertex4step = {
    graph + ("step", index -> step.index)
  }

  def containsKey(key: Any): Boolean = ???
  def get[T](key: Any): T = {
    println("get: step=" + step + ",key=" + key)
    vertex4step.vertices(Direction.OUT, "keyAndValue").forEachRemaining(v => {
      //println("get: step=" + step + ",key=" + key + ",v=" + v.toCC[KeyAndValue])
      if (v.toCC[KeyAndValue].key.get.equals(key)) {
        println("get: step=" + step + ",key=" + key + ",value=" + v.toCC[KeyAndValue].value.get)
        return v.toCC[KeyAndValue].value.get.asInstanceOf[T]
      }
    })
    return predecessor.get(key)
  }

  def put(key: Any, value: Any) = {
    println("put: step=" + step + ",key=" + key + ",value=" + value)
    val keyAndValue = graph + KeyAndValue(Option(key), Option(value))
    vertex4step --- "keyAndValue" --> keyAndValue
  }

  def remove(key: Any): Unit = ???
  def rename(oldKey: Any, newKey: Any): Unit = ???

}

