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
import nl.dgl.logces.TransferItemCountBetweenPallets
import nl.dgl.logces.SrcPallet
import nl.dgl.logces.DstPallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.logces.AnyPalletWithArticle
import nl.dgl.logces.ThePalletWithCode
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.SelectThePalletWithCode
import nl.dgl.logces.PalletSelector

class BijStortVoorbereiding {

  val process = Process {
    SelectAnyPalletWithArticle ~> //
      Step(xnge => {
        val palletWithArticle = xnge.get[Pallet](AnyPalletWithArticle)
        val bijstortAmount = xnge.get[Double](BijstortAmount)
        val itemAmount = palletWithArticle.article.weight_kg
        val bijstortItemCount = (bijstortAmount / itemAmount).toInt
        val bijstortScoopAmount = bijstortAmount - (bijstortItemCount * palletWithArticle.article.weight_kg)
        xnge.put(TransferItemCountBetweenPallets, bijstortItemCount)
        xnge.put(BijstortScoopAmount, bijstortScoopAmount)
      }) ~> //
      SelectThePalletWithCode ~> //
      Step(xnge => {
        val srcPallet = xnge.get[Pallet](AnyPalletWithArticle)
        val dstPallet = xnge.get[Pallet](ThePalletWithCode)
        xnge.put(SrcPallet, srcPallet)
        xnge.put(DstPallet, dstPallet)
      }) ~> //
      TransferItemsBetweenPallets
  }

}

object BijstortAmount {}
object BijstortScoopAmount {}

//////////////////////////////////////////
//////////////////////////////////////////
//////////////////////////////////////////

object MES_4C_LIENT extends App {

  // environment

  val product1 = Product("P1")

  val article_P1_10 = Article("A0", product1, 10.0);
  val article_P1_20 = Article("A1", product1, 20.0);

  Pallet("WarehousePallet1", article_P1_10, 100)
  Pallet("BijstortPallet1", article_P1_20, 50)

  // runtime

  val bsv = new BijStortVoorbereiding();
  new BijStortVoorbereidingView(bsv);

  val xnge = new Exchange();

  xnge.put(PalletSelector, PalletScanner(0))
  xnge.put(PalletCode, "BijstortPallet1")
  xnge.put(Article, article_P1_10)
  xnge.put(BijstortAmount, 101.101)

  bsv.process.process(xnge)

  println("TransferItemCountBetweenPallets=" + xnge.get(TransferItemCountBetweenPallets))
  println("BijstortScoopAmount=" + xnge.get(BijstortScoopAmount))

}
