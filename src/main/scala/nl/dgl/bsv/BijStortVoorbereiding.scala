package nl.dgl.bsv

import java.util.HashMap
import java.time.Instant
import java.util.UUID
import java.util.ArrayList
import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Select
import nl.dgl.logces.Pallet
import nl.dgl.logces.Pallets
import nl.dgl.logces.Article
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.Process
import nl.dgl.logces.Scanner
import nl.dgl.logces.Product
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.Lot
import nl.dgl.logces.Vessel
import nl.dgl.logces.Scale
import nl.dgl.logces.TransferProductBetweenVessels.AmountMarginPercent
import nl.dgl.logces.Vessels
import nl.dgl.ptb.dsl.Split
import nl.dgl.ptb.dsl.SelectSource
import nl.dgl.ptb.dsl.SelectFilter
import nl.dgl.logces.VesselSelectFiler
import nl.dgl.ptb.dsl.Selector
import nl.dgl.logces.VesselSelector
import nl.dgl.logces.LoGcEs
import nl.dgl.ptb.dsl.DSL

class BijStortVoorbereiding(implicit aPalletSelector: PalletSelector, aVesselSelector: VesselSelector) extends Process({ //
  Split(BSV.Bijstort, Process { //
    Step(xnge => {
      val product = xnge.get[Ingedient](BSV.Bijstort).product
      xnge.put(LoGcEs.Product, product)
    }) ~>
      Select(Pallets Where LoGcEs.Product) ~>
      Step(BsvSetupTransferIngredientFromAnyPalletToBsvPallet) ~> //
      Step(TransferItemsBetweenPallets) ~> //
      Select(VitamineBakken Where LoGcEs.Product) ~>
      Step(BsvSetupTransferIngredientFromAnyVesselToBsvVessel) ~>
      Step(TransferProductBetweenVessels) ~>
      Step(xnge => {
        val bijstortPallet = xnge.get[Pallet](LoGcEs.DstPallet)
        val bijstortScoopAmount = xnge.get[Double](TransferProductBetweenVessels.AmountActual)
        xnge.put(BSV.BijstortResultaat, (bijstortPallet.totalArticleWeight_kg, bijstortScoopAmount))
      })
  })

})

object BSV {

  val Bijstort = "Bijstort"
  val BijstortLijst = Bijstort + "List"
  val BijstortResultaat = Bijstort + "Result"
  val BijstortResultaaten = Bijstort + "ResultList"

}

case class Ingedient(product: Product, amount: Double) {}

/////////////

object BsvSetupTransferIngredientFromAnyPalletToBsvPallet extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val palletWithProduct = xnge.get[Pallet](DSL.Selection)
    // compute number of bags to transfer to new bsv pallet
    val bijstortAmount = xnge.get[Ingedient](BSV.Bijstort).amount
    val itemAmount = palletWithProduct.article.weight_kg
    val bijstortItemCount = (bijstortAmount / itemAmount).toInt
    xnge.put(TransferItemsBetweenPallets.Count, bijstortItemCount)
    // create a new bijstort pallet
    val bijstortPalletId = "pallet:" + UUID.randomUUID()
    val bijstortPallet = Pallet(bijstortPalletId);
    // prepare for the pallet-transfer step
    val srcPallet = palletWithProduct
    val dstPallet = bijstortPallet
    xnge.put(LoGcEs.SrcPallet, srcPallet)
    xnge.put(LoGcEs.DstPallet, dstPallet)
  }
}

////////////////

object BsvSetupTransferIngredientFromAnyVesselToBsvVessel extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val vesselWithProduct = xnge.get[Vessel](DSL.Selection)
    // compute amount of KG to transfer to new scoop bag
    val product = xnge.get[Product](LoGcEs.Product)
    val bijstortAmount = xnge.get[Ingedient](BSV.Bijstort).amount
    val bijstortPallet = xnge.get[Pallet](LoGcEs.DstPallet)
    val bijstortScoopAmount = bijstortAmount - (bijstortPallet.totalArticleWeight_kg)
    // create a new scoop bag
    val bijstortVessel = Vessel("vessel:schepzak:" + UUID.randomUUID(), product) // print ook label ...
    println("Weeg " + bijstortScoopAmount + " kg van " + product + " af uit " + vesselWithProduct + " en schep in " + bijstortVessel)
    // prepare the vessel-transfer step.
    xnge.put(LoGcEs.SrcVessel, vesselWithProduct)
    xnge.put(LoGcEs.DstVessel, bijstortVessel)
    xnge.put(TransferProductBetweenVessels.AmountTarget, bijstortScoopAmount)
  }

}

////////////////////////

object VitamineBakken extends SelectSource[Vessel] {

  def Where(xngeKey: String): SelectFilter[Vessel] = { new VesselSelectFiler(this, xngeKey) }
  def candidates(xnge: Exchange): List[Vessel] = {
    // this is the root-select-source so there is no xnge-key-value to select by
    // so we want the current list of them all.
    return Vessels.vessels.toList
  }
}

