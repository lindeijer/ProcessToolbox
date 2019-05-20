package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange


case class Product(code:String) {} 

object Product {
  
  private val products = ListBuffer(new Product(""));

  def apply(code:String):Product = {
    products.find(_.code.equals(code)).getOrElse({
      val product = new Product(code);
      products += product
      product
    })
  }
  
}

/**
 * An Article is a 'packaged' commodity with a fixed weight (which can be handled and sold).
 * 
 * Note: an Item is an instance of the concept Article.  
 */
case class Article(val code : String, val product:Product,val weight_kg:Double) {}

object Article {
  
  private val articles = ListBuffer(new Article("",Product(""),0.0));

  def apply(code:String,product:Product,weight_kg:Double):Article = {
    articles.find(_.code.equals(code)).foreach(article => {
        if (article.product.equals(product) && article.weight_kg.equals(weight_kg)) {
          throw new IllegalArgumentException("Article aleady exists: code="+code);
        }  
    });
    val article = new Article(code,product,weight_kg); 
    articles += article
    return article;
  }
  
  def apply(code:String):Article = {
    articles.find(_.code.equals(code)).getOrElse({
       throw new IllegalArgumentException("Unknown Article code="+code);
    })
  }

}

/**
 * An instance of an Article and an actual amount of a commodity which can he handled.
 */
case class Item(val article:Article)

/////////////// -------

/**
 * Contains a number of indistinguishable articles.
 */
case class Pallet(val code:String,article:Article) {
  
  var itemCount:Int = 0;
  
  def totalArticleWeight_kg() = {
    itemCount * article.weight_kg;
  }

}

object Pallet {
   
  private val pallets = ListBuffer(new Pallet("",Article("")));
   
  def apply(code:String,article:Article,itemCount:Int) = {
    pallets.find(_.code.equals(code)).foreach(pallet => {
      if (pallet.article.equals(article)) {
        throw new IllegalArgumentException("Pallet aleady exists: code="+code + ",article="+article);
      }
    })
    val pallet = new Pallet(code,article);
    pallet.itemCount = itemCount;
    pallets += pallet
    pallet
  }
  
  def apply(barcode:String) = {
     pallets.find(_.code.equals(barcode));
  }
  
  def destroy(pallet:Pallet) = {
     pallets.-=(pallet)  
  }  
   
  def transfer(src:Pallet,dst:Pallet,transferCount:Int) {
    System.out.println("Pallet.transfer: before src="+src+",dst="+dst+",transferCount="+transferCount);
    src.itemCount = src.itemCount - transferCount
    dst.itemCount = dst.itemCount + transferCount
    System.out.println("Pallet.transfer: after src="+src+",dst="+dst);
       
  }
  
  val rndm = new Random

  def random() = {
    val r = rndm.nextInt(pallets.size);
    pallets(r)
  }
}

// ---------------

class ScanPallet extends Scan {
  
  /**
   * Sets ScanPallet using Scanner.
   */
  override def process(xnge:Exchange) = {
    super.process(xnge) 
    val palletBarcode = xnge.get(ScannerEvent).asInstanceOf[ScannerEvent].code
    xnge.put(ScanPallet,Pallet(palletBarcode).get)
  }
}

object ScanPallet {}

