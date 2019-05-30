package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.UUID

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
   * Gets the existing pallet with code, or creates an empty pallet with the code
   */
  def apply(barcode: String) = {
    pallets.find(_.code.equals(barcode)).getOrElse({
      new Pallet(barcode, Article.unknown)
    })
  }

  /**
   * Gets any existing pallet with article, or creates an empty pallet for the article
   */
  def apply(article: Article): Pallet = {
    pallets.find(_.article.equals(article)).getOrElse({
      new Pallet("pallet:" + UUID.randomUUID, article)
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

