package nl.dgl.bsv

import nl.dgl.logces.Product
import nl.dgl.logces.Article
import nl.dgl.logces.Pallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.PalletCode
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.Vessel
import nl.dgl.logces.SrcVessel
import nl.dgl.logces.DstVessel

object MES_4C_LIENT extends App {

  // environment

  val product1 = Product("P1")

  val article_P1_10 = Article("A0", product1, 10.0); // 10 kg of product in article-bag
  val article_P1_20 = Article("A1", product1, 20.0); // 20 kg of product in article-bag

  Pallet("WarehousePallet1", article_P1_10, 100) // 100 article-bags on the warehouse pallet
  Pallet("BijstortPallet1") // zero bags on the bijstort pallet

  // runtime

  val bsv = new BijStortVoorbereiding();
  new BijStortVoorbereidingView(bsv);

  val xnge = new Exchange();

  xnge.put(PalletSelector, PalletScanner(0))
  xnge.put(PalletCode, "BijstortPallet1")
  xnge.put(Article, article_P1_10)
  xnge.put(BijstortAmount, 101.101)

  bsv.process.process(xnge)

  println("TransferItemCountBetweenPallets=" + xnge.get(TransferItemsBetweenPallets.Count))
  println("BijstortScoopAmount=" + xnge.get(TransferProductBetweenVessels.AmountActual))

  println("SrcVessel=" + xnge.get[Vessel](SrcVessel))
  println("DstVessel=" + xnge.get[Vessel](DstVessel))

}
