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
import nl.dgl.logces.PalletCode
import nl.dgl.logces.SelectAnyPalletWithArticle
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.SrcPallet
import nl.dgl.logces.DstPallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.logces.AnyPalletWithArticle
import nl.dgl.logces.ThePalletWithCode
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.SelectThePalletWithCode
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.SrcVessel
import nl.dgl.logces.DstVessel
import nl.dgl.logces.Lot
import nl.dgl.logces.Vessel

class BijStortVoorbereiding {

  val process = Process {
    Step(SelectAnyPalletWithArticle) ~> //
      Step(xnge => {
        val palletWithArticle = xnge.get[Pallet](AnyPalletWithArticle)
        val bijstortAmount = xnge.get[Double](BijstortAmount)
        val itemAmount = palletWithArticle.article.weight_kg
        val bijstortItemCount = (bijstortAmount / itemAmount).toInt
        val bijstortScoopAmount = bijstortAmount - (bijstortItemCount * palletWithArticle.article.weight_kg)
        xnge.put(TransferItemsBetweenPallets.Count, bijstortItemCount)
        xnge.put(TransferProductBetweenVessels.Amount, bijstortScoopAmount)
      }) ~> //
      Step(SelectThePalletWithCode) ~> //
      Step(xnge => {
        val srcPallet = xnge.get[Pallet](AnyPalletWithArticle)
        val dstPallet = xnge.get[Pallet](ThePalletWithCode)
        xnge.put(SrcPallet, srcPallet)
        xnge.put(DstPallet, dstPallet)
      }) ~> //
      Step(TransferItemsBetweenPallets) ~>
      Step(xnge => {
        val article = xnge.get[Article](Article)
        val srcVessel = Vessel("vessel1", Lot("lot1", article.product, 10 * 1000))
        val dstVessel = Vessel("vessel1", article.product)
        xnge.put(SrcVessel, srcVessel)
        xnge.put(DstVessel, dstVessel)
      }) ~> //
      Step(TransferProductBetweenVessels)
  }

}

object BijstortAmount {}

