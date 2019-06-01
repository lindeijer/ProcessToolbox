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
  def selectAnyPalletWithArticle(article: Article): Pallet
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
      println("PalletScanner: not the right id=" + palletId + ", found scanEvent.code=" + scanEvent.code)
      selectWithId(palletId, scanner.scan())
    }
  }

  override def selectAnyPalletWithArticle(article: Article): Pallet = {
    println("PalletScanner: will scan a pallet with article=" + article)
    selectWithArticle(article, scanner.scan())
  }

  private def selectWithArticle(article: Article, scanEvent: ScannerEvent): Pallet = {
    if (Pallet.isCode(scanEvent.code)) {
      val pallet = Pallet(scanEvent.code)
      if (pallet.article.equals(article)) {
        return pallet
      } else {
        println("PalletScanner: not the right article=" + article + ", found pallet.article=" + pallet.article)
        selectWithArticle(article, scanner.scan())
      }
    } else {
      println("PalletScanner: not a pallet code=" + scanEvent.code)
      selectWithArticle(article, scanner.scan())
    }
  }
}

object PalletScanner {

  def apply(location: Int) = {
    new PalletScanner(location)
  }
}
// ----

class SelectAnyPalletWithArticle extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val article = xnge.get[Article](Article);
    val palletSelector = xnge.get[PalletSelector](PalletSelector);
    println("SelectAnyPalletWithArticle: will select any pallet with article=" + article + " using selector=" + palletSelector)
    xnge.remove(AnyPalletWithArticle)
    val pallet = palletSelector.selectAnyPalletWithArticle(article);
    println("SelectAnyPalletWithArticle: selected any pallet with article, pallet=" + pallet)
    xnge.put(AnyPalletWithArticle, pallet)
  }

}

object SelectAnyPalletWithArticle extends SelectAnyPalletWithArticle {}

object AnyPalletWithArticle {}

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

