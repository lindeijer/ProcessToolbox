package nl.dgl.bsv

import nl.dgl.logces.Product
import nl.dgl.logces.Article
import nl.dgl.logces.Pallet
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.PalletSelector
import nl.dgl.logces.PalletScanner
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.Vessel
import nl.dgl.logces.Lot
import java.util.UUID
import nl.dgl.ptb.dsl.Selector
import nl.dgl.logces.PalletScannerManiac
import nl.dgl.logces.Scale
import nl.dgl.logces.TransferProductBetweenVessels.AmountMarginPercent
import nl.dgl.ptb.ui.swing.ProcessOverView
import nl.dgl.ptb.ui.swing.ProcessOverView
import scala.swing.Frame
import nl.dgl.ptb.serialization.tinkerpop.ExchangeGremlin
import nl.dgl.logces.VesselSelector
import nl.dgl.logces.VesselScannerLoser
import nl.dgl.ptb.dsl.StepConstructionHelper
import nl.dgl.ptb.dsl.ExchangeHashMap
import org.apache.tinkerpop.shaded.minlog.Log
import nl.dgl.logces.LoGcEs
import scala.util.Success
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import nl.dgl.logces.PalletScannerLoser

object MES_4C_LIENT extends Frame with App {

  // Log.TRACE();

  title = "BijStortVoorbereiding"

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

  def randomID(): String = {
    UUID.randomUUID().toString().split("-")(0)
  }

  Vessel("vitaminebak:" + randomID, Lot("lot:" + randomID, product1, 10 * 1000))
  Vessel("vitaminebak:" + randomID, Lot("lot:" + randomID, product2, 10 * 1000))

  // runtime

  implicit val aPalletScanner = PalletScannerLoser(0) // PalletScannerManiac(0)
  implicit val aVesselScanner = VesselScannerLoser(0);
  // implicit val scale0 = Scale(0)

  val bsv = new BijStortVoorbereiding();
  val bsvView = new ProcessOverView(bsv);

  contents = bsvView

  open()

  // xnge

  val xnge = new ExchangeHashMap()
  // new ExchangeGremlin();

  println("!!!!!!!!!!!!!!!!!! xnge.index=" + xnge.getStepIndex())

  val bijstortLijst = List(Ingedient(product1, 101.101), Ingedient(product2, 202.202))
  xnge.put(BSV.BijstortLijst, bijstortLijst);

  // xnge.put(Scale, Scale(0))
  xnge.put(AmountMarginPercent, 10.0)
  bsv.start(xnge).andThen({
    case Success(xngeResult) => {
      println("TransferItemCountBetweenPallets=" + xngeResult.get(TransferItemsBetweenPallets.Count))
      println("BijstortScoopAmount=" + xngeResult.get(TransferProductBetweenVessels.AmountActual))
      println("SrcVessel=" + xngeResult.get[Vessel](LoGcEs.SrcVessel))
      println("DstVessel=" + xngeResult.get[Vessel](LoGcEs.DstVessel))
      println("bijstortResultaten=" + xngeResult.get[Map[Any, Any]](BSV.BijstortResultaaten))
    }
  })

}

object MES_4CLIENT_RESTART extends Frame with App {

  title = "BijStortVoorbereiding - RESTART"

  implicit val aPalletScanner = PalletScannerManiac(0)
  implicit val aVesselScanner = VesselScannerLoser(0);

  val bsv = new BijStortVoorbereiding();
  val bsvView = new ProcessOverView(bsv);

  contents = bsvView

  open()

  // val xnge = new ExchangeGremlin(19);
  val xnge = new ExchangeHashMap()

  bsv.start(xnge).andThen({
    case Success(xngeResult) => {
      println("TransferItemCountBetweenPallets=" + xngeResult.get(TransferItemsBetweenPallets.Count))
      println("BijstortScoopAmount=" + xngeResult.get(TransferProductBetweenVessels.AmountActual))

      println("SrcVessel=" + xngeResult.get[Vessel](LoGcEs.SrcVessel))
      println("DstVessel=" + xngeResult.get[Vessel](LoGcEs.DstVessel))

      println("bijstortResultaten=" + xngeResult.get[Map[Any, Any]](BSV.BijstortResultaaten))

    }

  })

  // result

}