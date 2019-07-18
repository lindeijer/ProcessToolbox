package nl.dgl.logces

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.UUID
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListMap
import scala.collection.mutable.Map
import nl.dgl.ptb.dsl.SelectSource
import nl.dgl.ptb.dsl.SelectFilter
import nl.dgl.ptb.dsl.Exchange

class LogisticUnit(val id: String) {}

/**
 * Contains a number of indistinguishable items of the same article.
 */
case class Pallet(override val id: String, var article: Article) extends LogisticUnit(id) {

  var itemCount: Int = 0;

  def totalArticleWeight_kg() = {
    itemCount * article.weight_kg;
  }

}

object Pallet {

  private val pallets: ListBuffer[Pallet] = ListBuffer.empty;

  def currentList(): List[Pallet] = {
    return pallets.toList;
  }

  def apply(id: String, article: Article, itemCount: Int) = {
    pallets.find(_.id.equals(id)).foreach(pallet => {
      if (pallet.article.equals(article)) {
        throw new IllegalArgumentException("Pallet aleady exists: id=" + id + ",article=" + article);
      }
    })
    val pallet = new Pallet(id, article);
    pallet.itemCount = itemCount;
    pallets += pallet
    pallet
  }

  /**
   * Gets the existing pallet with code, or creates an empty pallet with the code
   */
  def apply(id: String) = {
    pallets.find(_.id.equals(id)).getOrElse({
      val pallet = new Pallet(id, Article.unknown)
      pallets += pallet
      pallet
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
    println("Pallet.transfer: before src.totalArticleWeight_kg=" + src.totalArticleWeight_kg() + ",dst.totalArticleWeight_kg=" + dst.totalArticleWeight_kg());
    if (dst.article.equals(Article.unknown) && dst.itemCount == 0) {
      dst.article = src.article
    } else {
      throw new IllegalArgumentException("Destination Pallet has wrong article, found " + dst.article + " but expected " + src.article);
    }
    src.itemCount = src.itemCount - transferCount
    dst.itemCount = dst.itemCount + transferCount
    println("Pallet.transfer: after src.totalArticleWeight_kg=" + src.totalArticleWeight_kg + ",dst.totalArticleWeight_kg=" + dst.totalArticleWeight_kg);
  }

  def isCode(id: String): Boolean = {
    pallets.exists(_.id.equals(id))
  }

  val rndm = new Random

  def random() = {
    val r = rndm.nextInt(pallets.size);
    pallets(r)
  }
}

class PalletSelectFiler(source: SelectSource[Pallet], xngeKey: String) extends SelectFilter[Pallet] with SelectSource[Pallet] {

  // with SelectFilter

  def And(xngeKey: String): SelectFilter[Pallet] = {
    return new PalletSelectFiler(this, xngeKey)
  }

  // with SelectSource

  def Where(xngeKey: String): SelectFilter[Pallet] = {
    return And(xngeKey)
  }

  def candidates(xnge: Exchange): List[Pallet] = {
    val candidates = source.candidates(xnge)
    val xngeValue = xnge.get[Any](xngeKey)
    xngeValue match {
      case article: Article => return candidates.filter(_.article.equals(article))
      case product: Product => return candidates.filter(_.article.product.equals(product))
      case _                => return candidates
    }
  }
}

object Pallets extends SelectSource[Pallet] {

  def Where(xngeKey: String): SelectFilter[Pallet] = { new PalletSelectFiler(this, xngeKey) }
  def candidates(xnge: Exchange): List[Pallet] = {
    // this is the root-select-source so there is no xnge-key-value to select by
    // so we want the current list of them all.
    return Pallet.currentList()
  }
}

/////////////////////////////////////////////////////////

/**
 * A container for a product
 */
trait Vessel {

  var product: Product
  def amount: Double
  /**
   * return the new lot-amount
   */
  def add(amount: Double, lot: Lot): (Double, Lot)
  /**
   * return the new lot-amounts
   */
  def add(amounts: List[(Double, Lot)]): List[(Double, Lot)]
  /**
   * return the subtracted lot-amounts !! not the new lot-amounts
   */
  def subtract(amount: Double): List[(Double, Lot)]

}

object Vessel {

  def transfer(src: Vessel, dst: Vessel, amount: Double) = {

    println("Vessel.transfer: before src=" + src + ",dst=" + dst + ",amount=" + amount);
    println("Vessel.transfer: before src.amount=" + src.amount + ",dst.amount=" + dst.amount);
    if (dst.product.equals(Article.unknown) && dst.amount == 0) {
      dst.product = src.product
    } else if (dst.product.equals(src.product)) {
      // OK!
    } else {
      throw new IllegalArgumentException("Destination Pallet has wrong article, found " + dst.product + " but expected " + src.product);
    }
    dst.add(src.subtract(amount))
    println("Pallet.transfer: after src.amount=" + src.amount + ",dst.amount=" + dst.amount);
  }

  def apply(id: String, lot: Lot): VesselPure = {
    val v = new VesselPure(id, lot)
    Vessels.vessels += v
    return v
  }

  def apply(id: String, product: Product): VesselMixed = {
    val v = new VesselMixed(id, product)
    Vessels.vessels += v
    return v
  }

}

object Vessels {

  val vessels: ListBuffer[Vessel] = ListBuffer.empty

  def getAll(product: Product): List[Vessel] = {
    vessels.filter(v => v.product.equals(product)).toList
  }

}

class VesselSelectFiler(source: SelectSource[Vessel], xngeKey: String) extends SelectFilter[Vessel] with SelectSource[Vessel] {

  // with SelectFilter

  def And(xngeKey: String): SelectFilter[Vessel] = {
    return new VesselSelectFiler(this, xngeKey)
  }

  // with SelectSource

  def Where(xngeKey: String): SelectFilter[Vessel] = {
    return And(xngeKey)
  }

  def candidates(xnge: Exchange): List[Vessel] = {
    val candidates = source.candidates(xnge)
    val xngeValue = xnge.get[Any](xngeKey)
    xngeValue match {
      case article: Product => return candidates.filter(_.product.equals(article))
      case _                => return candidates
    }
  }
}

/**
 * Contains an amount of a single product from a single lot.
 */
case class VesselPure(override val id: String, lot: Lot) extends LogisticUnit(id) with Vessel {

  var product = lot.product

  private var amountPure: Double = lot.amount

  def amount: Double = amountPure

  def add(amount: Double, lot: Lot): (Double, Lot) = {
    if (!lot.equals(this.lot)) {
      throw new IllegalArgumentException("Total amount must remain pure from lot=" + this.lot + " therefore rejecting amount from another lot=" + lot)
    }
    amountPure += amount;
    return (amountPure, lot)
  }

  def add(amounts: List[(Double, Lot)]): List[(Double, Lot)] = {
    amounts.foreach(x => add(x._1, x._2)) // must all be of same lot
    return List((amount, lot))
  }

  def subtract(amount: Double): List[(Double, Lot)] = {
    add(-amount, lot)
    List((amount, lot))
  }

}

/**
 * Contains an amount of a single product from one or more lots.
 */
case class VesselMixed(override val id: String, var product: Product) extends LogisticUnit(id) with Vessel {

  private val lots2amount: Map[Lot, Double] = HashMap.empty

  def amount = lots2amount.values.sum

  def add(amount: Double, lot: Lot): (Double, Lot) = {
    val currentAmount = lots2amount.get(lot).getOrElse(0.0)
    val newAmount = currentAmount + amount
    lots2amount.put(lot, newAmount)
    (newAmount, lot)
  }

  def add(amounts: List[(Double, Lot)]): List[(Double, Lot)] = {
    amounts.foreach(x => add(x._1, x._2)) // must all be of same lot
    lots2amount.map(l2a => (l2a._2, l2a._1)).toList
  }

  /**
   * Subtracts a weighted part from every lot, because we assume a mix of all contained lots
   */
  def subtract(minAmount: Double): List[(Double, Lot)] = {
    val totalAmount = amount
    val lots2bstrct = lots2amount.mapValues(currAmount => (currAmount - ((currAmount / totalAmount) * minAmount)))
    lots2amount.map(l2a => (l2a._2, l2a._1)).toList
  }

}
