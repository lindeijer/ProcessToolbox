package nl.dgl.bsv

import nl.dgl.logces.Product
import nl.dgl.logces.Article
import nl.dgl.logces.Pallet
import nl.dgl.bsv.ui.swing.BijStortVoorbereidingView
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.Vessel
import nl.dgl.logces.SrcVessel
import nl.dgl.logces.DstVessel
import nl.dgl.logces.PalletId

object MES_4C_LIENT extends App {

  // environment

  val product1 = Product("P1")
  val product2 = Product("P2")

  val article_P1_10 = Article("A0", product1, 10.0); // 10 kg of product in article-bag
  val article_P1_20 = Article("A1", product1, 20.0); // 20 kg of product in article-bag

  val article_P2_30 = Article("A3", product2, 30.0); // 30 kg of product in article-bag
  val article_P2_40 = Article("A4", product2, 40.0); // 40 kg of product in article-bag

  Pallet("WarehousePallet1", article_P1_10, 100) // 100 article-bags on the warehouse pallet
  Pallet("WarehousePallet2", article_P2_40, 200) // 100 article-bags on the warehouse pallet

  // runtime

  val bsv = new BijStortVoorbereiding();
  new BijStortVoorbereidingView(bsv);

  // xnge

  val xnge = new Exchange();

  xnge.put(PalletSelector, PalletScanner(0))

  val bijstortLijst = List(BSV.Ingedient(product1, 101.101), BSV.Ingedient(product2, 202.202))
  xnge.put(BSV.BijstortLijst, bijstortLijst);

  bsv.process.process(xnge)

  // result

  val bijstortResultaten = xnge.get[Map[Any, Any]](BSV.BijstortResultaaten)

  println("TransferItemCountBetweenPallets=" + xnge.get(TransferItemsBetweenPallets.Count))
  println("BijstortScoopAmount=" + xnge.get(TransferProductBetweenVessels.AmountActual))

  println("SrcVessel=" + xnge.get[Vessel](SrcVessel))
  println("DstVessel=" + xnge.get[Vessel](DstVessel))

  println("bijstortResultaten=" + bijstortResultaten)

}
