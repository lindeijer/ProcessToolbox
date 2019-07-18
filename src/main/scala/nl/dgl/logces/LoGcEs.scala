package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.Selector

object LoGcEs {

  val SrcPallet = "SrcPallet"
  val DstPallet = "DstPallet"
  val SrcVessel = "SrcVessel"
  val DstVessel = "DstVessel"
  val Product = "Product"

}

object TransferItemsBetweenPallets extends (Exchange => Unit) {

  val Count = "Count"

  def apply(xnge: Exchange) = {
    val srcPallet = xnge.get[Pallet](LoGcEs.SrcPallet)
    val dstPallet = xnge.get[Pallet](LoGcEs.DstPallet)
    val transferItemsBetweenPalletsCount = xnge.get[Int](TransferItemsBetweenPallets.Count)
    Pallet.transfer(srcPallet, dstPallet, transferItemsBetweenPalletsCount)
  }
}

// ----

trait PalletSelector extends Selector[Pallet] {
  //def selectThePalletWithId(palletId: String): Pallet
  //def selectAnyPalletWithProduct(product: Product): Pallet
}

object PalletSelector {}

// ----

class PalletScanner(location: Int) extends PalletSelector {

  val scanner = Scanner(location)

  def selectThePalletWithId(palletId: String): Pallet = {
    println("PalletScanner: will scan a pallet with id=" + palletId)
    selectWithId(palletId, scanner.scan())
  }

  private def selectWithId(palletId: String, scanEvent: ScannerEvent): Pallet = {
    if (scanEvent.code.equals(palletId)) {
      return Pallet(palletId)
    } else {
      println("PalletScanner: not the right id, required " + palletId + " but found barcode " + scanEvent.code)
      selectWithId(palletId, scanner.scan())
    }
  }

  def selectAnyPalletWithProduct(product: Product): Pallet = {
    println("PalletScanner: will scan a pallet with " + product)
    selectWithProduct(product, scanner.scan())
  }

  private def selectWithProduct(product: Product, scanEvent: ScannerEvent): Pallet = {
    if (Pallet.isCode(scanEvent.code)) {
      val pallet = Pallet(scanEvent.code)
      if (pallet.article.product.equals(product)) {
        return pallet
      } else {
        println("PalletScanner: not the right product, required " + product + " but found pallet with " + pallet.article.product)
        selectWithProduct(product, scanner.scan())
      }
    } else {
      println("PalletScanner: not a pallet code=" + scanEvent.code)
      selectWithProduct(product, scanner.scan())
    }
  }
}

object PalletScanner {

  def apply(location: Int) = {
    new PalletScanner(location)
  }
}

// =====================================

import java.util.concurrent._
import gremlin.scala.label

case class PalletScannerManiac(location: Int) extends PalletSelector {

  val scanner = Scanner(location)

  def init() = {
    val ex = new ScheduledThreadPoolExecutor(1)
    val task = new Runnable {
      def run() = {
        val selectedPallet = selectAnyPellet()
        println("selectedPallet=" + selectedPallet)
        listeners.foreach(_.apply(selectedPallet))
      }
    }
    val f = ex.scheduleAtFixedRate(task, 1, 5, TimeUnit.SECONDS)
  }

  init()

  private def selectAnyPellet(): Pallet = {
    selectAnyPellet(scanner.scan())
  }

  private def selectAnyPellet(scanEvent: ScannerEvent): Pallet = {
    if (Pallet.isCode(scanEvent.code)) {
      Pallet(scanEvent.code)
    } else {
      println("PalletScanner: not a pallet code=" + scanEvent.code)
      selectAnyPellet(scanner.scan())
    }
  }

}

////////////////////////////////////////////////

object TransferProductBetweenVessels extends (Exchange => Unit) {

  val AmountTarget = "AmountTarget"
  val AmountActual = "AmountActual"
  val AmountMarginPercent = "AmountMarginPercent"

  def apply(xnge: Exchange) = {
    val srcVessel = xnge.get[Vessel](LoGcEs.SrcVessel)
    val dstVessel = xnge.get[Vessel](LoGcEs.DstVessel)
    val transferAmountTarget = xnge.get[Double](TransferProductBetweenVessels.AmountTarget)
    val scale = Scale(0) // xnge.get[Scale](Scale)
    val marginPercent = xnge.get[Double](AmountMarginPercent)
    //
    val weighedAmount = scale.weigh(transferAmountTarget, marginPercent).amount
    println("TransferProductBetweenVessels: transferAmountTarget=" + transferAmountTarget + ",marginPercent=" + marginPercent + ",weighedAmount=" + weighedAmount)
    xnge.put(TransferProductBetweenVessels.AmountActual, weighedAmount)
    //
    println("TransferProductBetweenVessels: before; srcVessel.amount=" + srcVessel.amount + ",dstVessel.amount=" + dstVessel.amount)
    Vessel.transfer(srcVessel, dstVessel, weighedAmount)
    println("TransferProductBetweenVessels: after; srcVessel.amount=" + srcVessel.amount + ",dstVessel.amount=" + dstVessel.amount)
  }
}

//////////////

trait VesselSelector extends Selector[Vessel] {}

case class VesselScannerLoser(location: Int) extends VesselSelector {

}
