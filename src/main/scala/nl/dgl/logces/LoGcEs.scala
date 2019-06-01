package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Step

object SrcPallet {}
object DstPallet {}

class TransferItemsBetweenPallets extends (Exchange => Unit) {

  object Count {}

  def apply(xnge: Exchange) = {
    val srcPallet = xnge.get[Pallet](SrcPallet)
    val dstPallet = xnge.get[Pallet](DstPallet)
    val transferItemsBetweenPalletsCount = xnge.get[Int](TransferItemsBetweenPallets.Count)
    Pallet.transfer(srcPallet, dstPallet, transferItemsBetweenPalletsCount)
  }
}

object TransferItemsBetweenPallets extends TransferItemsBetweenPallets {}

// ----

trait PalletSelector {
  def selectThePalletWithId(palletId: String): Pallet
  def selectAnyPalletWithProduct(product: Product): Pallet
}

object PalletSelector {}

// ----

class PalletScanner(location: Int) extends PalletSelector {

  val scanner = Scanner(location)

  override def selectThePalletWithId(palletId: String): Pallet = {
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

  override def selectAnyPalletWithProduct(product: Product): Pallet = {
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
// ----

class SelectAnyPalletWithProduct extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val product = xnge.get[Product](Product);
    val palletSelector = xnge.get[PalletSelector](PalletSelector);
    println("SelectAnyPalletWithProduct: will select any pallet with " + product + " using selector=" + palletSelector)
    xnge.remove(AnyPalletWithProduct)
    val pallet = palletSelector.selectAnyPalletWithProduct(product);
    println("SelectAnyPalletWithProduct: selected any pallet with " + pallet.article)
    xnge.put(AnyPalletWithProduct, pallet)
  }

}

object SelectAnyPalletWithProduct extends SelectAnyPalletWithProduct {}

object AnyPalletWithProduct {}

// ----

class SelectThePalletWithId extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val palletId = xnge.get[String](PalletId)
    val palletSelector = xnge.get[PalletSelector](PalletSelector);
    println("SelectThePalletWithId: will select the pallet with id=" + palletId + " using selector=" + palletSelector)
    xnge.remove(ThePalletWithId)
    val pallet = palletSelector.selectThePalletWithId(palletId)
    println("SelectThePalletWithId: selected the pallet with id, pallet=" + pallet)
    xnge.put(ThePalletWithId, pallet)
  }
}

object SelectThePalletWithId extends SelectThePalletWithId {}

object ThePalletWithId {}
object PalletId {}

////////////////////////////////////////////////

class TransferProductBetweenVessels extends (Exchange => Unit) {

  object AmountTarget {}
  object AmountActual {}
  object AmountMarginPercent {}

  def apply(xnge: Exchange) = {
    val srcVessel = xnge.get[Vessel](SrcVessel)
    val dstVessel = xnge.get[Vessel](DstVessel)
    val transferAmountTarget = xnge.get[Double](TransferProductBetweenVessels.AmountTarget)
    val scale = xnge.get[Scale](Scale)
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

object TransferProductBetweenVessels extends TransferProductBetweenVessels {}

object SrcVessel {}
object DstVessel {}

