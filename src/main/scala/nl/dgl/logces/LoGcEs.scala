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
  def selectThePalletWithCode(code: String): Pallet
  def selectAnyPalletWithArticle(article: Article): Pallet
}

object PalletSelector {}

// ----

class PalletScanner(location: Int) extends PalletSelector {

  val scanner = Scanner(location)

  override def selectThePalletWithCode(code: String): Pallet = {
    println("PalletScanner: will scan a pallet with code=" + code)
    selectWithCode(code, scanner.scan())
  }

  private def selectWithCode(code: String, scanEvent: ScannerEvent): Pallet = {
    if (Pallet.isCode(scanEvent.code)) {
      return Pallet(scanEvent.code)
    } else {
      println("PalletScanner: not a pallet code=" + scanEvent.code)
      selectWithCode(code, scanner.scan())
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
        println("PalletScanner: not with article pallet=" + pallet)
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

class SelectThePalletWithCode extends (Exchange => Unit) {

  def apply(xnge: Exchange) = {
    val code = xnge.get[String](PalletCode)
    val palletSelector = xnge.get[PalletSelector](PalletSelector);
    println("SelectThePalletWithCode: will select the pallet with code=" + code + " using selector=" + palletSelector)
    xnge.remove(ThePalletWithCode)
    val pallet = palletSelector.selectThePalletWithCode(code)
    println("SelectThePalletWithCode: selected the pallet with code, pallet=" + pallet)
    xnge.put(ThePalletWithCode, pallet)
  }
}

object SelectThePalletWithCode extends SelectThePalletWithCode {}

object ThePalletWithCode {}
object PalletCode {}

////////////////////////////////////////////////

class TransferProductBetweenVessels extends (Exchange => Unit) {

  object Amount {}

  def apply(xnge: Exchange) = {
    val srcVessel = xnge.get[Vessel](SrcVessel)
    val dstVessel = xnge.get[Vessel](DstVessel)
    val transferAmount = xnge.get[Double](TransferProductBetweenVessels.Amount)
    println("TransferProductBetweenVessels: before; srcVessel.amount=" + srcVessel.amount + ",dstVessel.amount=" + dstVessel.amount)
    Vessel.transfer(srcVessel, dstVessel, transferAmount)
    println("TransferProductBetweenVessels: after; srcVessel.amount=" + srcVessel.amount + ",dstVessel.amount=" + dstVessel.amount)
  }
}

object TransferProductBetweenVessels extends TransferProductBetweenVessels {}

object SrcVessel {}
object DstVessel {}

