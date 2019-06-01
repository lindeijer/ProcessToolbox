package nl.dgl.bsv

import java.util.HashMap
import java.time.Instant
import java.util.UUID
import java.util.ArrayList
import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.Pallet
import nl.dgl.logces.Article
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.Process
import nl.dgl.logces.Scanner
import nl.dgl.logces.Product
import nl.dgl.ptb.ui.swing.ProcessSwingView
import nl.dgl.logces.PalletId
import nl.dgl.logces.SelectAnyPalletWithProduct
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.SrcPallet
import nl.dgl.logces.DstPallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.logces.AnyPalletWithProduct
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
import nl.dgl.logces.SelectThePalletWithId
import nl.dgl.logces.ThePalletWithId

class BijStortVoorbereiding {

  val bsvIngredientPalletZakken = Process { //
    Step(SelectAnyPalletWithProduct) ~> //
      Step(xnge => {
        val palletWithArticle = xnge.get[Pallet](AnyPalletWithProduct)
        val bijstortAmount = xnge.get[Double](BijstortAmount)
        val itemAmount = palletWithArticle.article.weight_kg
        val bijstortItemCount = (bijstortAmount / itemAmount).toInt
        xnge.put(TransferItemsBetweenPallets.Count, bijstortItemCount)
      }) ~> //
      Step(SelectThePalletWithId) ~> //
      Step(xnge => {
        val srcPallet = xnge.get[Pallet](AnyPalletWithProduct)
        val dstPallet = xnge.get[Pallet](ThePalletWithId)
        xnge.put(SrcPallet, srcPallet)
        xnge.put(DstPallet, dstPallet)
      }) ~>
      Step(TransferItemsBetweenPallets)
  }

  val bsvIngredientSchepzakken = Process { //
    Step(xnge => {
      val product = xnge.get[Product](Product)
      val bijstortAmount = xnge.get[Double](BijstortAmount)
      val bijstortPallet = xnge.get[Pallet](DstPallet)
      val bijstortScoopAmount = bijstortAmount - (bijstortPallet.totalArticleWeight_kg)
      //
      val vitamineBak = Vessel("vitamineBak1", Lot("lot1", product, 10 * 1000)) // wordt door operator gepakt
      val schepZak = Vessel("schepzak1", product) // print ook label ...
      println("Weeg " + bijstortScoopAmount + " kg van " + product + " af uit " + vitamineBak + " en schep in " + schepZak)
      //
      xnge.put(SrcVessel, vitamineBak)
      xnge.put(DstVessel, schepZak)
      xnge.put(Scale, Scale(0))
      xnge.put(AmountMarginPercent, 10.0)
      xnge.put(TransferProductBetweenVessels.AmountTarget, bijstortScoopAmount)
    }) ~> //
      Step(TransferProductBetweenVessels)
  }

  val process = Process { //
    bsvIngredientPalletZakken ~> bsvIngredientSchepzakken
  }

}

object BijstortAmount {}

