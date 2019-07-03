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

import gremlin.scala._
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.apache.tinkerpop.gremlin.structure.Direction
import org.apache.tinkerpop.shaded.kryo.Kryo
import java.io.OutputStream
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoMapper
import java.io.FileOutputStream
import java.io.File
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoWriter
import org.apache.tinkerpop.gremlin.structure.io.IoCore
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerFactory

@label("keyandvalue")
case class KeyAndValue(key: Option[Any], value: Option[Any])

object ExchangeGremlin {

  val tinkerGraph = TinkerGraph.open

  val graph: ScalaGraph = {
    if (java.nio.file.Files.isRegularFile(java.nio.file.Paths.get("ExchangeGremlin.kryo"))) {
      println("data EXISTS")
      tinkerGraph.io(gryo()).readGraph("ExchangeGremlin.kryo")
    } else {
      println("data NEW")
    }
    tinkerGraph.asScala
  }

  def commit() = {

    println("data COMITTED")

    val file = new File("ExchangeGremlin.kryo");
    val fos = new FileOutputStream(file);
    val gryoMapperBuilder = GryoMapper.build()
    val gryoMapper = gryoMapperBuilder //
      .addCustom(Class.forName("scala.collection.immutable.$colon$colon")) //
      .addCustom(Class.forName("scala.collection.immutable.Nil$")) //
      .addCustom(Class.forName("scala.collection.mutable.ListBuffer")) //
      .addCustom(classOf[nl.dgl.bsv.BSV$Ingedient]) //
      .addCustom(classOf[nl.dgl.logces.Product]) //
      .addCustom(classOf[nl.dgl.logces.Product$]) //
      .addCustom(classOf[nl.dgl.logces.TransferProductBetweenVessels$AmountMarginPercent$]) //
      .addCustom(classOf[nl.dgl.logces.Pallet]) //
      .addCustom(Class.forName("nl.dgl.logces.Pallet$")) //
      .addCustom(classOf[nl.dgl.logces.Article]) //
      .addCustom(classOf[nl.dgl.ptb.dsl.Selection$]) //
      .addCustom(classOf[nl.dgl.logces.TransferItemsBetweenPallets$Count$]) //
      .addCustom(Class.forName("nl.dgl.logces.SrcPallet$")) //
      .addCustom(Class.forName("nl.dgl.logces.DstPallet$")) //
      .addCustom(classOf[nl.dgl.logces.VesselPure]) //
      .addCustom(classOf[nl.dgl.logces.Lot]) //
      .addCustom(Class.forName("nl.dgl.logces.SrcVessel$")) //
      .addCustom(Class.forName("nl.dgl.logces.DstVessel$")) //
      .addCustom(classOf[nl.dgl.logces.VesselMixed]) //
      .addCustom(Class.forName("scala.collection.mutable.HashMap")) //
      .addCustom(Class.forName("nl.dgl.logces.TransferProductBetweenVessels$AmountTarget$")) //
      .addCustom(Class.forName("nl.dgl.logces.TransferProductBetweenVessels$AmountActual$")) //
      .addCustom(Class.forName("scala.Tuple2$mcDD$sp")) //
      .addCustom(Class.forName("scala.collection.immutable.Map$Map2")) //
      .create
    val writer = GryoWriter.build().mapper(gryoMapper).create()
    writer.writeGraph(fos, tinkerGraph)

    println("data COMITTED")

  }

  commit();

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
    ExchangeGremlin.commit()
  }

  def remove(key: Any): Unit = ???
  def rename(oldKey: Any, newKey: Any): Unit = ???

}

