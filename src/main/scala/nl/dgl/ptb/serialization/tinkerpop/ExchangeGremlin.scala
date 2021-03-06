package nl.dgl.ptb.serialization.tinkerpop

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import org.apache.tinkerpop.gremlin.structure.io.IoCore.gryo
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoMapper
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoReader
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoWriter
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.apache.tinkerpop.shaded.kryo.Serializer
import org.apache.tinkerpop.shaded.kryo.io.Input
import org.apache.tinkerpop.shaded.kryo.io.Output

import gremlin.scala.GraphAsScala
import gremlin.scala.ScalaGraph
import gremlin.scala.Vertex
import gremlin.scala.asScalaVertex
import gremlin.scala.label
import gremlin.scala.Key
import nl.dgl.bsv.Ingedient
import nl.dgl.logces.Article
import nl.dgl.logces.Lot
import nl.dgl.logces.Pallet
import nl.dgl.logces.Product
import nl.dgl.logces.VesselMixed
import nl.dgl.logces.VesselPure
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource
import org.apache.tinkerpop.gremlin.structure.io.AbstractIoRegistry
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoIo
import org.apache.tinkerpop.gremlin.structure.Graph
import org.apache.tinkerpop.shaded.kryo.Kryo
import nl.dgl.ptb.serialization.tinkerpop.GryoScalaCollectionSerializer
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.ExchangeHashMap
import nl.dgl.ptb.dsl.StepConstructionHelper
import nl.dgl.logces.LogisticLocation

@label("keyandvalue")
case class KeyAndValue(key: Option[Any], value: Option[Any])

////////////////////////////////////////////////////

class ProductSerializer extends Serializer[Product] {
  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            product: nl.dgl.logces.Product): Unit = //
    {
      output.writeString(product.code)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.Product]): nl.dgl.logces.Product = //
    {
      return Product(input.readString())
    }
}

class ArticleSerializer extends Serializer[Article] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            article: nl.dgl.logces.Article): Unit = //
    {
      output.writeString(article.code)
      kryo.writeObject(output, article.product)
      output.writeDouble(article.weight_kg)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.Article]): nl.dgl.logces.Article = //
    {
      return Article(input.readString(), kryo.readObject[Product](input, classOf[Product]), input.readDouble())
    }

}

class PalletSerializer extends Serializer[Pallet] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            pallet: nl.dgl.logces.Pallet): Unit = //
    {
      output.writeString(pallet.id)
      kryo.writeObject(output, pallet.article)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.Pallet]): nl.dgl.logces.Pallet = //
    {
      return Pallet(input.readString(), kryo.readObject[Article](input, classOf[Article]))
    }

}

class IngredientSerializer extends Serializer[Ingedient] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            ingedient: nl.dgl.bsv.Ingedient): Unit = //
    {
      kryo.writeObject(output, ingedient.product)
      output.writeDouble(ingedient.amount)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.bsv.Ingedient]): nl.dgl.bsv.Ingedient = //
    {
      return Ingedient(kryo.readObject[Product](input, classOf[Product]), input.readDouble())
    }

}

class VesselPureSerializer extends Serializer[VesselPure] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            vesselPure: nl.dgl.logces.VesselPure): Unit = //
    {
      output.writeString(vesselPure.id)
      kryo.writeObject(output, vesselPure.lot)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.VesselPure]): nl.dgl.logces.VesselPure = //
    {
      return VesselPure(input.readString(), kryo.readObject[Lot](input, classOf[Lot]))
    }

}

class LotSerializer extends Serializer[Lot] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            lot: nl.dgl.logces.Lot): Unit = //
    {
      output.writeString(lot.id)
      kryo.writeObject(output, lot.product)
      output.writeDouble(lot.amount)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.Lot]): nl.dgl.logces.Lot = //
    {
      return Lot(input.readString(), kryo.readObject[Product](input, classOf[Product]), input.readDouble())
    }

}

class VesselMixedSerializer extends Serializer[VesselMixed] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            vesselMixed: nl.dgl.logces.VesselMixed): Unit = //
    {
      output.writeString(vesselMixed.id)
      kryo.writeObject(output, vesselMixed.product)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.VesselMixed]): nl.dgl.logces.VesselMixed = //
    {
      return VesselMixed(input.readString(), kryo.readObject[Product](input, classOf[Product]))
    }

}

class LogisticLocationSerializer extends Serializer[LogisticLocation] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            logisticLocation: nl.dgl.logces.LogisticLocation): Unit = //
    {
      output.writeString(logisticLocation.id)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[nl.dgl.logces.LogisticLocation]): nl.dgl.logces.LogisticLocation = //
    {
      return LogisticLocation(input.readString())
    }

}

/////////////////////////////////////////////////

object ExchangeGremlinGyro {

  val tinkerGraph = TinkerGraph.open

  val gryoMapper =
    GryoMapper.build()
      .addCustom(Class.forName("scala.collection.immutable.$colon$colon"), new GryoScalaCollectionSerializer)
      .addCustom(Class.forName("scala.collection.immutable.Nil$")) //
      .addCustom(Class.forName("scala.collection.mutable.ListBuffer")) //
      .addCustom(Class.forName("scala.collection.mutable.HashMap")) //
      .addCustom(Class.forName("scala.Tuple2$mcDD$sp")) //
      .addCustom(Class.forName("scala.collection.immutable.Map$Map2")) //
      .addCustom(Class.forName("scala.collection.immutable.Map$Map1")) //
      //.addCustom(Class.forName("scala.Tuple2")) //
      //.addCustom(Class.forName("scala.Some")) //

      .addCustom(classOf[Product], new ProductSerializer()) //
      .addCustom(classOf[Ingedient], new IngredientSerializer()) //
      .addCustom(classOf[Pallet], new PalletSerializer()) //
      .addCustom(classOf[Article], new ArticleSerializer()) //
      .addCustom(classOf[VesselPure], new VesselPureSerializer()) //
      .addCustom(classOf[Lot], new LotSerializer()) //
      .addCustom(classOf[VesselMixed], new VesselMixedSerializer()) //
      .addCustom(classOf[LogisticLocation], new LogisticLocationSerializer()) //
      .create

  val graph: ScalaGraph = {
    if (java.nio.file.Files.isRegularFile(java.nio.file.Paths.get("ExchangeGremlin.kryo"))) {
      println("data EXISTS")
      val file = new File("ExchangeGremlin.kryo");
      val fis = new FileInputStream(file);
      tinkerGraph.io(gryo()).reader().mapper(gryoMapper).create().readGraph(fis, tinkerGraph)
    } else {
      println("data NEW")
    }
    tinkerGraph.asScala
  }

  def commit() = {
    val file = new File("ExchangeGremlin.kryo");
    val fos = new FileOutputStream(file);
    tinkerGraph.io(gryo()).writer().mapper(gryoMapper).create().writeGraph(fos, tinkerGraph);
    println("data COMITTED")
  }

  commit();

  def reconstructStep(int: Int): Vertex = {
    graph.V(int).fold().head().get(0)
  }

}

object ExchangeGremlin {

  val graph: ScalaGraph = ExchangeGremlinGyro.graph

  def commit() = ExchangeGremlinGyro.commit();

  def apply(): Exchange = {
    return ExchangeGremlin()

  }

}

class ExchangeGremlin private (stepIndex: Int, xngePrev: Exchange) extends Exchange {

  def this(stepIndex: Int) = this(stepIndex, new ExchangeHashMap()) // reconstruct process

  def this() = this(StepConstructionHelper.counter.getAndIncrement()) // construct process

  override def step(nextStepIndex: Int): Exchange = {
    return new ExchangeGremlin(nextStepIndex, this);
  }

  override def split(nextStepIndex: Int): Exchange = {
    return new ExchangeGremlin(nextStepIndex, this);
  }

  def getStepIndex(): Int = stepIndex

  override def getIsStepFinished(): Boolean = {
    this.getLocal[Boolean]("isFinished").getOrElse(false)
  }

  override def setStepIsFinished() = {
    this.put("isFinished", true)
  }

  implicit val graph = ExchangeGremlin.graph;

  val indexKey = Key[Int]("index")

  val vertex4step = {
    val v = graph.traversal.V().has("step", indexKey, stepIndex)
      .fold() // get or
      .coalesce(_.unfold[Vertex](), _.addV("step").property(indexKey, stepIndex)) // create
    val vertex = v.head();
    println("XNGE vertex4step: index=" + stepIndex + ",i=" + vertex.id());
    vertex
  }

  def containsKey(key: String): Boolean = ???

  def getLocal[T](key: String): Option[T] = {
    // println("XNGE getLocal: step.index=" + stepIndex + ",key=" + key + ",vertex4step=" + vertex4step)
    val kAvs = graph.traversal.V(vertex4step).out("keyAndValue").toList()
    // println("XNGE getLocal: step.index=" + stepIndex + ",key=" + key + ",kAvs=" + kAvs)
    kAvs.foreach(kAv => {
      // println("XNGE getLocal: step.index=" + stepIndex + ",kAv=" + kAv)
      if (kAv.toCC[KeyAndValue].key.get.equals(key)) {
        val value = kAv.toCC[KeyAndValue].value.get
        // println("XNGE getLocal: step.index=" + stepIndex + ",key=" + key + ",value=" + value)
        return Some(value.asInstanceOf[T])
      }
    })
    // println("XNGE getLocal: step.index=" + stepIndex + ",key=" + key + ",value=null")
    return None
  }

  override def get[T](key: String): Option[T] = {
    val valueLocalOpt = getLocal[T](key);
    if (valueLocalOpt.isDefined) { // is there a more idiomatic way?
      return valueLocalOpt;
    }
    return xngePrev.get[T](key);
  }

  override def put(key: String, value: Any) = {
    println("XNGE put: step.index=" + stepIndex + ",key=" + key + ",value=" + value)
    val keyAndValue = graph + KeyAndValue(Option(key), Option(value))
    vertex4step --- "keyAndValue" --> keyAndValue
    ExchangeGremlin.commit();
    // println("XNGE put check: get value=...")
    // println("XNGE put check: get value=" + get(key))
  }

  def remove(key: String): Unit = ???
  def rename(oldKey: String, newKey: String): Unit = ???

}

