package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Step

case class Product(code: String) {}

object Product {

  private val products: ListBuffer[Product] = ListBuffer.empty;

  def apply(code: String): Product = {
    products.find(_.code.equals(code)).getOrElse({
      val product = new Product(code);
      products += product
      product
    })
  }

  val unknown = new Product("???")

}

/**
 * An Article is a 'packaged' commodity with a fixed weight (which can be handled and sold).
 *
 * Note: an Item is an instance of the concept Article.
 */
case class Article(val code: String, val product: Product, val weight_kg: Double) {}

object Article {

  private val articles: ListBuffer[Article] = ListBuffer.empty;

  def apply(code: String, product: Product, weight_kg: Double): Article = {
    articles.find(_.code.equals(code)).foreach(article => {
      if (article.product.equals(product) && article.weight_kg.equals(weight_kg)) {
        throw new IllegalArgumentException("Article aleady exists: code=" + code);
      }
    });
    val article = new Article(code, product, weight_kg);
    articles += article
    return article;
  }

  def apply(code: String): Article = {
    articles.find(_.code.equals(code)).getOrElse({
      throw new IllegalArgumentException("Unknown Article code=" + code);
    })
  }

  val unknown = new Article("", Product.unknown, 0.0)

}

/**
 * An instance of an Article and an actual amount of a commodity which can he handled.
 */
case class Item(val article: Article)

/////////////// -------

/**
 * Contains a number of indistinguishable articles.
 */
case class Pallet(val code: String, article: Article) {

  var itemCount: Int = 0;

  def totalArticleWeight_kg() = {
    itemCount * article.weight_kg;
  }

}

object Pallet {

  private val pallets: ListBuffer[Pallet] = ListBuffer.empty;

  def apply(code: String, article: Article, itemCount: Int) = {
    pallets.find(_.code.equals(code)).foreach(pallet => {
      if (pallet.article.equals(article)) {
        throw new IllegalArgumentException("Pallet aleady exists: code=" + code + ",article=" + article);
      }
    })
    val pallet = new Pallet(code, article);
    pallet.itemCount = itemCount;
    pallets += pallet
    pallet
  }

  /**
   * Gets the pallet with code, or creates an empty pallet with the code
   */
  def apply(barcode: String) = {
    pallets.find(_.code.equals(barcode)).getOrElse({
      new Pallet(barcode, Article.unknown)
    })
  }

  def destroy(pallet: Pallet) = {
    pallets.-=(pallet)
  }

  def transfer(src: Pallet, dst: Pallet, transferCount: Int) {
    println("Pallet.transfer: before src=" + src + ",dst=" + dst + ",transferCount=" + transferCount);
    src.itemCount = src.itemCount - transferCount
    dst.itemCount = dst.itemCount + transferCount
    println("Pallet.transfer: after src=" + src + ",dst=" + dst);

  }

  def isCode(code: String): Boolean = {
    pallets.exists(_.code.equals(code))
  }

  val rndm = new Random

  def random() = {
    val r = rndm.nextInt(pallets.size);
    pallets(r)
  }
}

///////////////////////////////////////////////////

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