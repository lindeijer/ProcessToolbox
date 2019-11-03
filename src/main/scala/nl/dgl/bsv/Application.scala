package nl.dgl.bsv

import java.io.File
import java.util.UUID
import java.util.concurrent._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.Frame
import scala.util.Success

import org.http4s._
import org.http4s.MediaType
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.server.blaze._
import org.http4s.StaticFile
import org.http4s.server.staticcontent.FileService

import cats.effect._
import cats.implicits._
import nl.dgl.logces.Article
import nl.dgl.logces.LoGcEs
import nl.dgl.logces.LogisticLocation
import nl.dgl.logces.Lot
import nl.dgl.logces.Pallet
import nl.dgl.logces.PalletScannerLoser
import nl.dgl.logces.PalletScannerManiac
import nl.dgl.logces.Product
import nl.dgl.logces.TransferItemsBetweenPallets
import nl.dgl.logces.TransferProductBetweenVessels
import nl.dgl.logces.TransferProductBetweenVessels.AmountMarginPercent
import nl.dgl.logces.Vessel
import nl.dgl.logces.VesselScannerLoser
import nl.dgl.ptb.dsl.DSL
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.ExchangeHashMap
import nl.dgl.ptb.ui.swing.ProcessOverView

import cats.effect._
// import cats.effect._

import cats.implicits._
// import cats.implicits._

import org.http4s.server.blaze.BlazeServerBuilder
// import org.http4s.server.blaze.BlazeServerBuilder

import org.http4s.server.staticcontent._
// import org.http4s.server.staticcontent._

import org.http4s.syntax.kleisli._
// import org.http4s.syntax.kleisli._

object MES_4_CLIENT extends Frame with IOApp {

  val blockingEc = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  val pathToDist = "../../../workspace-ember/ember-quickstart2/dist"
  val pathToIndex = pathToDist + "/index.html"

  val ccc = new File(pathToIndex)

  println("ccc.canRead()=" + ccc.canRead())
  println("ccc.getAbsolutePath()xx=" + ccc.getAbsolutePath())

  val theService = HttpRoutes.of[IO] {
    //case GET -> Root / "hello" / name         => Ok(s"Hello, $name.")
    case GET -> Root / "actions" => Ok(getJsonApi(), `Content-Type`(MediaType.application.`vnd.api+json`))
    //case request @ GET -> Root / "index.html" => StaticFile.fromFile(new File(pathToIndex), blockingEc, Some(request)).getOrElseF(NotFound()) // In case the file doesn't exist
  }.orNotFound

  import org.http4s.server.middleware._

  val corsService = CORS(theService)

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      //.withHttpApp(helloWorldService)
      .withHttpApp(corsService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  //////////////////

  def getJsonApi(): String = {
    return JsonApi.getProcessJsonApi(bsv.asInstanceOf[nl.dgl.ptb.dsl.Process]);
  }

  //////////////////////////////

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

  val vitamineHal = LogisticLocation("VitamineHal")

  val aPalletScanner = PalletScannerLoser(vitamineHal) //PalletScannerManiac(vitamineHal)

  val aVesselScanner = VesselScannerLoser(vitamineHal);

  val bsv = new BijStortVoorbereiding();
  val bsvView = new ProcessOverView(bsv);

  contents = bsvView

  open()

  // xnge

  val xnge = Exchange()

  xnge.put(DSL.Location, vitamineHal)

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

  val vitamineHal = LogisticLocation("VitamineHal")

  val aPalletScanner = PalletScannerManiac(vitamineHal)
  val aVesselScanner = VesselScannerLoser(vitamineHal);

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