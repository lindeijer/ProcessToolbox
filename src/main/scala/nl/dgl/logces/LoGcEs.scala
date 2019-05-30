package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Step

object SrcPallet {}
object DstPallet {}
object TransferItemCountBetweenPallets {}

class TransferItemsBetweenPallets extends Step {

  override def step(xnge: Exchange) = {
    val srcPallet = xnge.get[Pallet](SrcPallet)
    val dstPallet = xnge.get[Pallet](DstPallet)
    val transferItemCountBetweenPallets = xnge.get[Int](TransferItemCountBetweenPallets)
    Pallet.transfer(srcPallet, dstPallet, transferItemCountBetweenPallets)
  }
}

object TransferItemsBetweenPallets extends TransferItemsBetweenPallets {}

// ----

trait Selector[T] {
  def select(code: String): T
}

trait PalletSelector extends Selector[Pallet] {
  def selectWithArticle(article: Article): Pallet
}

class PalletScanner(location: Int) extends PalletSelector {

  val scanner = Scanner(location)

  override def select(code: String): Pallet = {
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

  def selectWithArticle(article: Article): Pallet = {
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

class SelectAnyPalletWithArticle(palletSelector: PalletSelector) extends Step {

  override def step(xnge: Exchange) = {
    val article = xnge.get[Article](Article);
    println("SelectAnyPalletWithArticle: will select any pallet with article=" + article)
    xnge.remove(AnyPalletWithArticle)
    val pallet = palletSelector.selectWithArticle(article)
    println("SelectAnyPalletWithArticle: selected any pallet with article, pallet=" + pallet)
    xnge.put(AnyPalletWithArticle, pallet)
  }

}

object AnyPalletWithArticle {}

object ScanAnyPalletWithArticle extends SelectAnyPalletWithArticle(new PalletScanner(0)) {

}

// ----

class SelectThePalletWithCode(palletSelector: PalletSelector) extends Step {

  override def step(xnge: Exchange) = {
    val code = xnge.get[String](PalletCode)
    println("SelectThePalletWithCode: will select the pallet with code=" + code)
    xnge.remove(ThePalletWithCode)
    val pallet = palletSelector.select(code)
    println("SelectThePalletWithCode: selected the pallet with code, pallet=" + pallet)
    xnge.put(ThePalletWithCode, pallet)
  }
}

object ThePalletWithCode {}

object ScanThePalletWithCode extends SelectThePalletWithCode(new PalletScanner(0)) {}

object PalletCode {}