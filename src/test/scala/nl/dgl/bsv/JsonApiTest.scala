package nl.dgl.bsv

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import nl.dgl.logces.PalletSetup

class JsonApiTest extends FlatSpec with Matchers {

  "JsonApi" should "produce JSON:API data" in {
    PalletSetup.article_P1_10;
    val bsv = new BijStortVoorbereiding();
    val json = JsonApi.getProcessJsonApi(bsv)
    println("json=" + json)
  }

}