package nl.dgl.logces_dsl

import nl.dgl.ptb.dsl.ExchangeHashMap
import nl.dgl.logces.LoGcEs
import nl.dgl.logces.Product
import nl.dgl.ptb.dsl.Split
import nl.dgl.bsv.BSV
import nl.dgl.ptb.dsl.Select
import nl.dgl.logces.Pallets
import scala.util.Success

import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.concurrent.Await
import scala.concurrent._
import scala.concurrent.duration._
import nl.dgl.logces.PalletSetup
import nl.dgl.logces.Pallet

class PalletSelectSpec extends FlatSpec with Matchers {

  val aPalletSetup = new PalletSetup();

  "A Pallets Select Filter" should "select a sub-set of all pallets with a particular product" in {
    val palletsAllList = Pallets.candidates(null)
    val palletsWhereProduct = Pallets Where LoGcEs.Product
    val xnge = new ExchangeHashMap()
    xnge.put(LoGcEs.Product, Product("P1"))
    val candidates = palletsWhereProduct.candidates(xnge);
    assert(candidates.size < palletsAllList.size)
  }

}

