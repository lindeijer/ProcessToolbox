package nl.dgl.bsv

import nl.dgl.logces.Product
import nl.dgl.logces.Article
import nl.dgl.logces.Pallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.PalletCode
import nl.dgl.logces.TransferItemCountBetweenPallets

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
