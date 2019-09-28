package nl.dgl.ptb.dsl

import org.scalatest.AsyncFlatSpec
import java.util.UUID

object Boo {

  val Galoo = "Boogaloo"
  val GalooLijst = Galoo + "List"
  val GalooResultaat = Galoo + "Result"
  val GalooResultaaten = Galoo + "ResultList"

}

class ActionSplitSpec extends AsyncFlatSpec {

  behavior of "ActionSplit"

  it should "split the sub-process based on the item-list during start invocation" in {
    val splitBoogaloo = Split(
      Boo.Galoo, //
      Step(xnge => {
        val uuid = xnge.get[UUID](Boo.Galoo).get
        xnge.put(Boo.GalooResultaat, uuid)
      }))
    val xnge = new ExchangeHashMap()
    val uuid1 = UUID.randomUUID();
    val uuid2 = UUID.randomUUID();
    val uuid3 = UUID.randomUUID();
    xnge.put(Boo.GalooLijst, List(uuid1, uuid2, uuid3)) //
    splitBoogaloo.start(xnge);
    assert(splitBoogaloo.futureSteps.value.get.get.size == 3)
  }

  it should "eventually complete the future exchange with the accumulated results" in {
    val splitBoogaloo = Split(
      Boo.Galoo, //
      Step(xnge => {
        val uuid = xnge.get[UUID](Boo.Galoo).get
        xnge.put(Boo.GalooResultaat, uuid)
      }))
    val xnge = new ExchangeHashMap()
    val uuid1 = UUID.randomUUID();
    val uuid2 = UUID.randomUUID();
    val uuid3 = UUID.randomUUID();
    val boogaloos = List(uuid1, uuid2, uuid3)
    xnge.put(Boo.GalooLijst, boogaloos) //
    splitBoogaloo.start(xnge) map {
      xnge => assert(allBoogalooItemsCopiedToResults(xnge))
    }
  }

  def allBoogalooItemsCopiedToResults(xnge: Exchange): Boolean = {
    val boogaloos = xnge.get[List[UUID]](Boo.GalooLijst).get
    val uuids = xnge.get[Map[Any, Any]](Boo.GalooResultaaten).get
    if (boogaloos.size != uuids.size) {
      return false;
    }
    uuids.forall(kv => { kv._1 equals kv._2 })
  }

}