package test

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

import com.romix.scala.serialization.kryo.ScalaCollectionSerializer
import org.apache.tinkerpop.shaded.kryo.Kryo.DefaultInstantiatorStrategy
import org.apache.tinkerpop.shaded.objenesis.strategy.StdInstantiatorStrategy

import gremlin.scala.GraphAsScala
import gremlin.scala.ScalaGraph
import gremlin.scala.Vertex
import gremlin.scala.asScalaVertex
import gremlin.scala.label

import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource
import org.apache.tinkerpop.gremlin.structure.io.AbstractIoRegistry
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoIo
import org.apache.tinkerpop.gremlin.structure.Graph
import org.apache.tinkerpop.shaded.kryo.Kryo
import java.util.UUID
import org.apache.tinkerpop.shaded.kryo.serializers.DefaultSerializers
import org.apache.tinkerpop.shaded.kryo.serializers.FieldSerializer
import nl.dgl.tinkerpop.shaded.kryo.serializers.GryoScalaCollectionSerializer
import com.romix.scala.serialization.kryo.ScalaCollectionSerializer

object TestGremlinGyro extends App {

  val bbbbbb = GryoIo.build();

  val gryoMapper = {
    val resultGryoMapper = GryoMapper.build()
      .addCustom(classOf[scala.collection.immutable.$colon$colon[_]], new GryoScalaCollectionSerializer) //
      .addCustom(Class.forName("scala.collection.immutable.Nil$")) //
      .addCustom(classOf[XYZ], new XYZSerializer()) //
      .create
    resultGryoMapper
  }

  val tinkerA = TinkerGraph.open
  val graphA: ScalaGraph = tinkerA.asScala

  val fileName = "Test." + UUID.randomUUID().toString() + ".kryo";

  def writeGraphToFile() = {
    val file = new File(fileName);
    val fos = new FileOutputStream(file);
    GryoWriter.build().mapper(gryoMapper).create().writeGraph(fos, tinkerA);
    println("data WRITTEN")
  }

  val tinkerB = TinkerGraph.open
  val graphB: ScalaGraph = tinkerA.asScala

  def readGraphFromFile() = {
    val file = new File(fileName);
    val fis = new FileInputStream(file);
    GryoReader.build().mapper(gryoMapper).create().readGraph(fis, tinkerB);
    println("data READ")
  }

  def testBefore() = {
    implicit val graph = graphA;
    val paris = graph + "Paris"
    println("paris=" + paris);
    val key = "KEY"
    val value = "VALUE"
    val xngeValue1 = graph + ExchangeValue(Option("VALUE"))
    paris --- key --> xngeValue1
    //
    val xngeValue2 = graph + ExchangeValue(Option(XYZ("MyFirstProduct")))
    println("xngeValue2=" + xngeValue2);
    paris --- "HasProduct" --> xngeValue2
    //
    val trky = List("ABC", "DEF")
    val trkyVrtx = graph + ExchangeValue(Option(trky))
    println("xngeValue2=" + xngeValue2);
    paris --- trky --> trkyVrtx
  }

  def testAfter() = {
    implicit val graph = graphB;
    val vertexes = graph.V().toList()
    println("vertexes=" + vertexes);
    //
    println("vertexes(0).keys=" + vertexes(0).keys());
    println("vertexes(0).valueMap=" + vertexes(0).valueMap);
    println("vertexes(0).outE=" + vertexes(0).outE().toList());
    println("vertexes(0).out(HasProduct)=" + vertexes(0).out("HasProduct").toList());
    println("vertexes(0).HasProduct=" + vertexes(0).out("HasProduct").map(_.toCC[ExchangeValue].value.get).toList())
    //
    println("vertexes(1).valueMap=" + vertexes(1).valueMap);
    println("vertexes(1).inE=" + vertexes(1).inE().toList());
  }

  testBefore();
  writeGraphToFile()
  readGraphFromFile();
  testAfter();

}

@label("ExchangeValue")
case class ExchangeValue(value: Option[Any])

case class XYZ(code: String) {

}

class XYZSerializer extends org.apache.tinkerpop.shaded.kryo.Serializer[XYZ] {

  def write(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, output: Output, //
            xyz: XYZ): Unit = //
    {
      output.writeString(xyz.code)
    }

  def read(kryo: org.apache.tinkerpop.shaded.kryo.Kryo, input: Input, //
           clazz: Class[XYZ]): XYZ = //
    {
      return XYZ(input.readString())
    }
}

