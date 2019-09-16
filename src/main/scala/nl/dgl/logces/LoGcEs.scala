package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.Selector
import java.util.concurrent._
import gremlin.scala.label
import scala.concurrent.Promise
import nl.dgl.ptb.dsl.Location

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

  override def getSelectType() = classOf[Pallet]

}

/**
 * This loser never scans a pallet
 */
case class PalletScannerLoser(location: LogisticLocation) extends PalletSelector {

  override def select(candidates: List[Pallet], selectionPromise: Promise[Pallet]) = {
    // the loser never fulfills his promise.
  }

  override def getSelectLocation() = location

}

case class PalletScannerManiac(location: LogisticLocation) extends PalletSelector {

  val scanner = Scanner(location)

  override def getSelectLocation() = location

  val ex = new ScheduledThreadPoolExecutor(1)

  override def select(candidates: List[Pallet], selectionPromise: Promise[Pallet]) = {
    val task: Runnable = new Runnable {
      def run(): Unit = {
        val selectedPallet = selectAnyPellet(scanner.scan())
        if (selectionPromise.isCompleted) {
          return
        }
        if (candidates.contains(selectedPallet)) {
          selectionPromise.success(selectedPallet)
          return
        }
        select(candidates, selectionPromise);
      }
    }
    ex.schedule(task, 5, TimeUnit.SECONDS)
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

trait VesselSelector extends Selector[Vessel] {

  override def getSelectType() = classOf[Vessel]

}

case class VesselScannerLoser(location: LogisticLocation) extends VesselSelector {

  override def select(candidates: List[Vessel], selectionPromise: Promise[Vessel]) = {
    // the loser never fulfills his promise.
  }

  override def getSelectLocation() = location

}
