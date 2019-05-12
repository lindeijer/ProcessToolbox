package nl.dgl.bsv

import java.util.HashMap
import java.time.Instant
import java.util.UUID
import java.util.ArrayList
import scala.collection.mutable.ListBuffer
import scala.util.Random
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.logces.Pallet
import nl.dgl.logces.Article
import nl.dgl.logces.ScanPallet
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.Process
import nl.dgl.logces.Scanner
import nl.dgl.logces.Product

object BijStortVoorbereiding extends App  {
  
  val process = Process {
      ScanThePalletWithCode ~> ScanAnyPalletWithArticle ~> TransferItemsBetweenPallets
  }
  
  println("Hello bijstort: Transfer Goal")
  
  val product1 = Product("P1")
  
  val article0 = Article("A0",product1,10.0);
  val article1 = Article("A1",product1,10.0);
  
  Pallet("WarehousePallet1",article1,100)
  Pallet("BijstortPallet1",article0,100)
    
  val in = new Exchange();
  in.put(Scanner, Scanner(1))
  in.put(PalletCode,"BijstortPallet1")
  in.put(Article,article1)

  process.process(in)

  
}  



/////////////////////////////////////

class TransferItemsBetweenPallets extends Step {

  override def process(xnge:Exchange) = {
    val srcPallet = xnge.get(ScanAnyPalletWithArticle).asInstanceOf[Pallet]
    val dstPallet = xnge.get(ScanThePalletWithCode).asInstanceOf[Pallet]
    val transferCount = 1; // xnge.get("transferCount").asInstanceOf[Int]
    Pallet.transfer(srcPallet, dstPallet,transferCount)
  }
}

object TransferItemsBetweenPallets extends TransferItemsBetweenPallets {
  
}

/////////////////// ----


// ----

object ScanAnyPalletWithArticle extends ScanPallet {
  
  override def process(xnge:Exchange) = {
    val article = xnge.get(Article).asInstanceOf[Article]
    println("ScanPalletWithArticle: Looking for any Pallet with article="+article)
    xnge.remove(ScanAnyPalletWithArticle)
    while (!xnge.containsKey(ScanAnyPalletWithArticle)) {
      super.process(xnge)
      val pallet = xnge.get(ScanPallet).asInstanceOf[Pallet]
      if (pallet.article.equals(article)) {
        println("ScanPalletWithArticle: Found " + pallet + " with article="+pallet.article)
        xnge.put(ScanAnyPalletWithArticle,pallet)
      } else {
        println("ScanPalletWithArticle: Wrong " + pallet + " with article="+pallet.article)
      }
    }
    
  }
}



// ----

object ScanThePalletWithCode extends ScanPallet {
  override def process(xnge:Exchange) = {
    val code = xnge.get(PalletCode).asInstanceOf[String]
    println("ScanThePalletWithCode: Looking for the Pallet with code="+code)
    xnge.remove(ScanThePalletWithCode)
    while (!xnge.containsKey(ScanThePalletWithCode)) {
      super.process(xnge)
      val pallet = xnge.get(ScanPallet).asInstanceOf[Pallet]
      if (pallet.code.equals(code)) {
        println("ScanThePalletWithCode: Found "+ pallet)
        xnge.put(ScanThePalletWithCode,pallet)
      } else {
        println("ScanThePalletWithCode: Wrong " + pallet)
      }
    }
  }
}

object PalletCode {}


// ===========================


