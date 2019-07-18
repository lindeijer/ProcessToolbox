package nl.dgl.bsv

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import org.apache.tinkerpop.gremlin.structure.io.IoCore.gryo
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoMapper
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoReader
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoWriter
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerFactory
import org.apache.tinkerpop.gremlin.structure.io.IoRegistry
import org.apache.tinkerpop.gremlin.process.traversal.{ Order, P }
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerFactory

import gremlin.scala.GraphAsScala
import gremlin.scala.ScalaGraph
import gremlin.scala.dsl.DomainRoot
import gremlin.scala._

import nl.dgl.ptb.dsl.KeyAndValue
import nl.dgl.logces.Pallet
import nl.dgl.logces.Article
import nl.dgl.logces.Product

//class ColorSerializer extends Serializer[Pallet] {
//   public void write (Kryo kryo, Output output, Color color) {
//      output.writeInt(color.getRGB());
//   }
//
//   public Color read (Kryo kryo, Input input, Class<? extends Color> type) {
//      return new Color(input.readInt());
//   }
//}

class ProductSerializer extends org.apache.tinkerpop.shaded.kryo.Serializer[Product] {

  def read( //
    x$1:   org.apache.tinkerpop.shaded.kryo.Kryo,
    input: org.apache.tinkerpop.shaded.kryo.io.Input,
    x$3:   Class[nl.dgl.logces.Product]): nl.dgl.logces.Product = {
    return Product(input.readString())
  }

  def write( //
    x$1:     org.apache.tinkerpop.shaded.kryo.Kryo,
    output:  org.apache.tinkerpop.shaded.kryo.io.Output,
    product: nl.dgl.logces.Product): Unit = {
    output.writeString(product.code)
  }

}

case class XYZ(i: Int, s: String)

object BEFORE {

  // Log.TRACE();

  val tinkerGraph = TinkerGraph.open

  val gyroMapper = {
    GryoMapper.build()
      //.addCustom(Class.forName("scala.collection.immutable.$colon$colon")) //
      //.addCustom(Class.forName("scala.collection.immutable.Nil$")) //
      //.addCustom(Class.forName("scala.collection.mutable.ListBuffer")) //
      //.addCustom(Class.forName("scala.collection.mutable.HashMap")) //
      //.addCustom(Class.forName("scala.Tuple2$mcDD$sp")) //
      //.addCustom(Class.forName("scala.collection.immutable.Map$Map2")) //
      // .addCustom(classOf[Pallet]) //
      // .addCustom(classOf[Article]) //
      .addCustom(classOf[Product], new ProductSerializer()) //
      //.addCustom(classOf[nl.dgl.bsv.XYZ]) // , new PalletSerializer
      .create
  }

  val graph: ScalaGraph = tinkerGraph.asScala

  def commit() = {
    val file = new File("XYZ.kryo");
    val fos = new FileOutputStream(file);
    val writer = GryoWriter.build().mapper(gyroMapper).create()
    writer.writeGraph(fos, tinkerGraph)
    println("data COMITTED")
  }

  commit();

}

object AFTER {

  // Log.TRACE();

  val tinkerGraph = TinkerGraph.open

  val graph: ScalaGraph = {
    println("data EXISTS")
    val file = new File("XYZ.kryo");
    val fis = new FileInputStream(file);
    val reader = GryoReader.build().mapper(BEFORE.gyroMapper).create()
    reader.readGraph(fis, tinkerGraph)
    tinkerGraph.asScala
  }

}

object TEST_AA extends App {

  val Name = Key[String]("name")
  val Age = Key[Int]("age")

  {
    implicit val graphBefore = BEFORE.graph

    graphBefore + ("Person", Name -> "marko", Age -> 29)
    graphBefore + ("Person", Name -> "josh", Age -> 21)

    println("graphBefore=" + graphBefore)

    BEFORE.commit()

  }

  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

  {
    implicit val g = AFTER.graph

    println("g=" + g)

    val peopleTraversal = for {
      person <- g.V.hasLabel("Person")
    } yield (person)

    val peopleList = peopleTraversal.toList()

    println("peopleList=" + peopleList);

  }
}

object TEST_BB extends App {

  //  Log.DEBUG();
  //Log.TRACE();

  //Log.debug("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");
  val palletKey = "pallet"

  {
    implicit val graphBefore = BEFORE.graph

    val vertex4step = graphBefore + ("Step") asScala

    val product1 = Product("P1")
    val article_P1_10 = Article("A-P1-10", product1, 10.0); // 10 kg of P1 in article-bag
    val pallet1 = Pallet("WarehousePallet1", article_P1_10, 100) // 100 article-bags on the warehouse pallet

    val keyAndValue = graphBefore + KeyAndValue(Option(palletKey), Option(product1))

    vertex4step --- "keyAndValue" --> keyAndValue

    println("graphBefore=" + graphBefore)

    BEFORE.commit()

  }

  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

  {
    implicit val g = AFTER.graph

    println("g=" + g)

    def getLocal[T](key: Any): Option[T] = {
      val kAvs = g.V.out("keyAndValue").toList()
      kAvs.foreach(kAv => {
        if (kAv.toCC[KeyAndValue].key.get.equals(key)) {
          val value = kAv.toCC[KeyAndValue].value.get
          return Some(value.asInstanceOf[T])
        }
      })
      return None
    }

    val pallet = getLocal[Pallet](palletKey)

    println("pallet=" + pallet);

  }
}

object TEST_XX extends App {

  println("-------------------->")

  implicit val graph = TinkerFactory.createModern.asScala
  val g = graph.traversal

  g.V //all vertices
  g.E //all edges

  g.V(1).outE("knows") //follow outgoing edges
  g.V(1).out("knows") //follow outgoing edges to incoming vertex

  val weight = Key[Double]("weight")
  for {
    person <- g.V.hasLabel("person")
    favorite <- person.outE("likes").order(By(weight, Order.decr)).limit(1).inV
  } yield (person, favorite.label)

  // remove all people over 30 from the g - also removes corresponding edges
  val Age = Key[Int]("age")
  g.V.hasLabel("person").has(Age, P.gte(30)).drop.iterate

  println("+++++++++++++++++++++++++")

  import gremlin.scala._
  import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph

  // Keys for properties which can later be used for type safe traversals
  val Founded = Key[String]("founded")
  val Distance = Key[Int]("distance")

  // create labelled vertex
  val paris = graph + "Paris"

  // create vertex with typed properties
  val london = graph + ("London", Founded -> "43 AD")

  // create labelled edges
  paris --- "OneWayRoad" --> london
  paris <-- "OtherWayAround" --- london
  paris <-- "Eurostar" --> london

  // create edge with typed properties
  paris --- ("Eurostar", Distance -> 495) --> london

  // type safe access to properties
  paris.out("Eurostar").value(Founded).head //43 AD
  paris.outE("Eurostar").value(Distance).head //495
  london.valueOption(Founded) //Some(43 AD)
  london.valueOption(Distance) //None
  paris.setProperty(Founded, "300 BC")

  val Name = Key[String]("name")

  val v1 = graph + ("person", Name -> "marko", Age -> 29) asScala

  v1.keys // Set(Key("name"), Key("age"))
  v1.property(Name) // "marko"
  v1.valueMap // Map("name" -> "marko", "age" -> 29)
  v1.valueMap("name", "age") // Map("name" -> "marko", "age" -> 29)

  println("======================================")

  import gremlin.scala._
  import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerFactory
  import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph

  // select all labelled steps
  g.V(1).as("a").outE.as("b").select.toList
  // returns a `(Vertex, Edge)` for each path

  // select subset of labelled steps
  val a = StepLabel[Vertex]()
  val b = StepLabel[Edge]()
  val c = StepLabel[Double]()

  val traversal = g.V(1).as(a).outE("created").as(b).value("weight").as(c)

  traversal.select((b, c)).head
  // returns a `(Edge, Double)`

  println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")

}

