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
import nl.dgl.ptb.ui.swing.ProcessSwingView
import nl.dgl.logces.PalletId
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.SrcPallet
import nl.dgl.logces.DstPallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.logces.ThePalletWithId
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.SrcVessel
import nl.dgl.logces.DstVessel
import nl.dgl.logces.Lot
import nl.dgl.logces.Vessel
import nl.dgl.logces.Scale
import nl.dgl.logces.TransferProductBetweenVessels.AmountMarginPercent
import nl.dgl.logces.Vessels
import nl.dgl.logces.ThePalletWithId
import nl.dgl.ptb.dsl.Split
import nl.dgl.ptb.dsl.SelectSource
import nl.dgl.ptb.dsl.SelectFilter
import nl.dgl.ptb.dsl.Selection

class BijStortVoorbereiding {

  val bsvIngredient = Process { //
    Step(xnge => {
      val product = xnge.get[BSV.Ingedient](BSV.Bijstort).product
      xnge.put(Product, product)
    }) ~>
      Select(Pallets Where Product) ~>
      Step(BsvSetupTransferIngredientFromAnyPalletToBsvPallet) ~> //
      Step(TransferItemsBetweenPallets) ~> //
      Select(VitamineBakken Where Product) ~>
      Step(BsvSetupTransferIngredientFromAnyVesselToBsvVessel) ~>
      Step(TransferProductBetweenVessels) ~>
      Step(xnge => {
        val bijstortPallet = xnge.get[Pallet](DstPallet)
        val bijstortScoopAmount = xnge.get[Double](TransferProductBetweenVessels.AmountActual)
        xnge.put(BSV.BijstortResultaat, (bijstortPallet.totalArticleWeight_kg, bijstortScoopAmount))
      })
  }

  val process = Process { //
    Split(BSV.Bijstort, bsvIngredient)
  }

}

object BSV {

  val Bijstort = "Bijstort"
  val BijstortLijst = Bijstort + "List"
  val BijstortResultaat = Bijstort + "Result"
  val BijstortResultaaten = Bijstort + "ResultList"

  case class Ingedient(product: Product, amount: Double) {}
}

/////////////

class BsvSetupTransferIngredientFromAnyPalletToBsvPallet extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val palletWithProduct = xnge.get[Pallet](Selection)
    // compute number of bags to transfer to new bsv pallet
    val bijstortAmount = xnge.get[BSV.Ingedient](BSV.Bijstort).amount
    val itemAmount = palletWithProduct.article.weight_kg
    val bijstortItemCount = (bijstortAmount / itemAmount).toInt
    xnge.put(TransferItemsBetweenPallets.Count, bijstortItemCount)
    // create a new bijstort pallet
    val bijstortPalletId = "pallet:" + UUID.randomUUID()
    val bijstortPallet = Pallet(bijstortPalletId);
    // prepare for the pallet-transfer step
    val srcPallet = palletWithProduct
    val dstPallet = bijstortPallet
    xnge.put(SrcPallet, srcPallet)
    xnge.put(DstPallet, dstPallet)
  }
}

object BsvSetupTransferIngredientFromAnyPalletToBsvPallet extends BsvSetupTransferIngredientFromAnyPalletToBsvPallet {}

////////////////

class BsvSetupTransferIngredientFromAnyVesselToBsvVessel extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val vesselWithProduct = xnge.get[Vessel](Selection)
    // compute amount of KG to transfer to new scoop bag
    val product = xnge.get[Product](Product)
    val bijstortAmount = xnge.get[BSV.Ingedient](BSV.Bijstort).amount
    val bijstortPallet = xnge.get[Pallet](DstPallet)
    val bijstortScoopAmount = bijstortAmount - (bijstortPallet.totalArticleWeight_kg)
    // create a new scoop bag
    val bijstortVessel = Vessel("vessel:schepzak:" + UUID.randomUUID(), product) // print ook label ...
    println("Weeg " + bijstortScoopAmount + " kg van " + product + " af uit " + vesselWithProduct + " en schep in " + bijstortVessel)
    // prepare the vessel-transfer step.
    xnge.put(SrcVessel, vesselWithProduct)
    xnge.put(DstVessel, bijstortVessel)
    xnge.put(TransferProductBetweenVessels.AmountTarget, bijstortScoopAmount)
  }

}

object BsvSetupTransferIngredientFromAnyVesselToBsvVessel extends BsvSetupTransferIngredientFromAnyVesselToBsvVessel {}

////////////////////////

object VitamineBakken extends SelectSource[Vessel] {

  def Where(xngeKey: Any): SelectFilter[Vessel] = { new VesselSelectFiler(this, xngeKey) }
  def candidates(xnge: Exchange): List[Vessel] = {
    // this is the root-select-source so there is no xnge-key-value to select by
    // so we want the current list of them all.
    return Vessels.vessels.toList
  }
}

class VesselSelectFiler(source: SelectSource[Vessel], xngeKey: Any) extends SelectFilter[Vessel] with SelectSource[Vessel] {

  // with SelectFilter

  def And(xngeKey: Any): SelectFilter[Vessel] = {
    return new VesselSelectFiler(this, xngeKey)
  }

  // with SelectSource

  def Where(xngeKey: Any): SelectFilter[Vessel] = {
    return And(xngeKey)
  }

  def candidates(xnge: Exchange): List[Vessel] = {
    val candidates = source.candidates(xnge)
    val xngeValue = xnge.get[Any](xngeKey)
    xngeValue match {
      case article: Product => return candidates.filter(_.product.equals(article))
      case _                => return candidates
    }
  }
}

