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
import nl.dgl.logces.Lot
import java.util.UUID
import nl.dgl.ptb.dsl.Selector
import nl.dgl.logces.PalletScannerManiac
import nl.dgl.logces.Scale
import nl.dgl.logces.TransferProductBetweenVessels.AmountMarginPercent

object MES_4C_LIENT extends App {

  // environment

  val product1 = Product("P1")
  val product2 = Product("P2")

  val article_P1_10 = Article("A-P1-10", product1, 10.0); // 10 kg of P1 in article-bag
  val article_P1_20 = Article("A-P1-20", product1, 20.0);
  val article_P1_30 = Article("A-P1-30", product1, 30.0);
  val article_P1_40 = Article("A-P1-40", product1, 40.0);

  val article_P2_30 = Article("A-P2-30", product2, 30.0); // 30 kg of P2 in article-bag
  val article_P2_40 = Article("A-P2-40", product2, 40.0);
  val article_P2_50 = Article("A-P2-50", product2, 50.0);
  val article_P2_60 = Article("A-P2-60", product2, 60.0);

  Pallet("WarehousePallet1", article_P1_10, 100) // 100 article-bags on the warehouse pallet
  Pallet("WarehousePallet2", article_P1_20, 100)
  Pallet("WarehousePallet3", article_P1_30, 100)
  Pallet("WarehousePallet4", article_P1_40, 100)
  Pallet("WarehousePallet5", article_P2_30, 200)
  Pallet("WarehousePallet6", article_P2_40, 200)
  Pallet("WarehousePallet7", article_P2_50, 200)
  Pallet("WarehousePallet8", article_P2_60, 200)

  Vessel("vessel:vitaminebak:" + UUID.randomUUID(), Lot("lot:" + UUID.randomUUID(), product1, 10 * 1000))
  Vessel("vessel:vitaminebak:" + UUID.randomUUID(), Lot("lot:" + UUID.randomUUID(), product2, 10 * 1000))

  // runtime

  val bsv = new BijStortVoorbereiding();
  new BijStortVoorbereidingView(bsv);

  // xnge

  val xnge = new Exchange();

  xnge.put(Selector, PalletScannerManiac(0)) //

  val bijstortLijst = List(BSV.Ingedient(product1, 101.101), BSV.Ingedient(product2, 202.202))
  xnge.put(BSV.BijstortLijst, bijstortLijst);

  xnge.put(Scale, Scale(0))
  xnge.put(AmountMarginPercent, 10.0)
  bsv.process.process(xnge)

  // result

  val bijstortResultaten = xnge.get[Map[Any, Any]](BSV.BijstortResultaaten)

  println("TransferItemCountBetweenPallets=" + xnge.get(TransferItemsBetweenPallets.Count))
  println("BijstortScoopAmount=" + xnge.get(TransferProductBetweenVessels.AmountActual))

  println("SrcVessel=" + xnge.get[Vessel](SrcVessel))
  println("DstVessel=" + xnge.get[Vessel](DstVessel))

  println("bijstortResultaten=" + bijstortResultaten)

}
