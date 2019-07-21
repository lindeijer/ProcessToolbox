/**
 * *****************************************************************************
 * Copyright 2012 Roman Levenstein
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ****************************************************************************
 */

package nl.dgl.ptb.serialization.tinkerpop

import scala.collection.Traversable
import org.apache.tinkerpop.shaded.kryo.Kryo
import org.apache.tinkerpop.shaded.kryo.io.Input
import org.apache.tinkerpop.shaded.kryo.io.Output
import com.romix.scala.serialization.kryo.ScalaCollectionSerializer
import org.apache.tinkerpop.shaded.kryo.Kryo.DefaultInstantiatorStrategy
import org.apache.tinkerpop.shaded.objenesis.strategy.StdInstantiatorStrategy
import org.apache.tinkerpop.shaded.kryo.Kryo.DefaultInstantiatorStrategy

class GryoScalaCollectionSerializer extends ScalaCollectionSerializer {

  override def read(kryo: Kryo, input: Input, typ: Class[Traversable[_]]): Traversable[_] = {

    val zzz = kryo.getInstantiatorStrategy.asInstanceOf[DefaultInstantiatorStrategy];
    if (zzz.getFallbackInstantiatorStrategy == null) {
      println("!!!! setFallbackInstantiatorStrategy(new StdInstantiatorStrategy())");
      zzz.setFallbackInstantiatorStrategy(new StdInstantiatorStrategy())
    }

    super.read(kryo, input, typ);
  }

  override def write(kryo: Kryo, output: Output, obj: Traversable[_]) = {
    super.write(kryo, output, obj);
  }

}