package test

import org.apache.tinkerpop.shaded.kryo.Kryo
import org.apache.tinkerpop.shaded.kryo.Serializer
import org.apache.tinkerpop.shaded.kryo.io.Output
import org.apache.tinkerpop.shaded.kryo.io.Input
import org.apache.tinkerpop.shaded.minlog.Log
import org.apache.tinkerpop.shaded.kryo.Kryo.DefaultInstantiatorStrategy
import org.apache.tinkerpop.shaded.objenesis.strategy.StdInstantiatorStrategy
import org.apache.tinkerpop.shaded.kryo.Kryo
import com.romix.scala.serialization.kryo.EnumerationSerializer
import com.romix.scala.serialization.kryo.ScalaImmutableSetSerializer
import com.romix.scala.serialization.kryo.ScalaCollectionSerializer
import com.romix.scala.serialization.kryo.ScalaImmutableMapSerializer
import java.io.FileInputStream
import java.io.FileOutputStream

import nl.dgl.ptb.serialization.tinkerpop.GryoScalaCollectionSerializer

object TestSerializeLIstKryo extends App {

  Log.TRACE();

  def aaa() = {

    val list = List("foo", "bar")

    val kryo = new Kryo(); // NOT SHADED

    //    // Serialization of Scala enumerations
    //kryo.addDefaultSerializer(classOf[scala.Enumeration#Value], classOf[EnumerationSerializer])
    //kryo.register(Class.forName("scala.Enumeration$Val"))
    //kryo.register(classOf[scala.Enumeration#Value])
    //
    //    // Serialization of Scala maps like Trees, etc
    //kryo.addDefaultSerializer(classOf[scala.collection.Map[_, _]], classOf[ScalaImmutableMapSerializer]) // ScalaImmutableMapSerializer
    //kryo.addDefaultSerializer(classOf[scala.collection.generic.MapFactory[scala.collection.Map]], classOf[ScalaImmutableMapSerializer])
    //
    //    // Serialization of Scala sets
    // kryo.addDefaultSerializer(classOf[scala.collection.Set[_]], classOf[ScalaImmutableSetSerializer])
    // kryo.addDefaultSerializer(classOf[scala.collection.generic.SetFactory[scala.collection.Set]], classOf[ScalaImmutableSetSerializer])
    //

    // Serialization of all Traversable Scala collections like Lists, Vectors, etc
    kryo.addDefaultSerializer(classOf[scala.collection.Traversable[_]], classOf[ScalaCollectionSerializer])
    //kryo.addDefaultSerializer(classOf[scala.collection.immutable.Set[_]], classOf[ScalaImmutableSetSerializer])

    kryo.register(classOf[scala.collection.immutable.$colon$colon[_]], new GryoScalaCollectionSerializer())
    //kryo.register(classOf[scala.collection.immutable.Nil$], 41)
    //kryo.addDefaultSerializer(classOf[scala.Enumeration#Value], classOf[EnumerationSerializer])
    //kryo.addDefaultSerializer(classOf[scala.collection.immutable.Set[_]], classOf[ScalaImmutableSetSerializer])
    //kryo.addDefaultSerializer(classOf[scala.collection.generic.SetFactory[scala.collection.Set]], classOf[ScalaImmutableSetSerializer])

    val output = new Output(new FileOutputStream("file.dat"));

    kryo.writeClassAndObject(output, list);
    output.close();

    println("Written now read!");

    val input = new Input(new FileInputStream("file.dat"));
    val theObject = kryo.readClassAndObject(input);
    input.close();

    println("theObject=" + theObject);

  }

  aaa()

}

