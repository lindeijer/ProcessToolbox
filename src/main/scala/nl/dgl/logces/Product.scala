package nl.dgl.logces

import scala.collection.mutable.ListBuffer

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

// ----

case class Lot(id: String, product: Product, amount: Double) {}

// ----

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

  val unknown = new Article("???", Product.unknown, 0.0)

}

// ----

/**
 * An instance of an Article and an actual amount of a commodity which can he handled.
 */
case class Item(val article: Article)

