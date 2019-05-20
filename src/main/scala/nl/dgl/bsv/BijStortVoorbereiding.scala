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
import nl.dgl.ptb.ui.swing.ProcessSwingView

object BijStortVoorbereiding extends App  {
  
  
  val process = Process {
      ScanThePalletWithCode ~> //
      ScanAnyPalletWithArticle ~> //
      Step (xnge => {
        val srcPallet = xnge.get(ScanAnyPalletWithArticle).asInstanceOf[Pallet]
        val dstPallet = xnge.get(ScanThePalletWithCode).asInstanceOf[Pallet]
        xnge.put(SrcPallet,srcPallet)
        xnge.put(DstPallet,dstPallet)
        
        val bijstortAmount = xnge.get(BijstortAmount).asInstanceOf[Double]
        val itemAmount = srcPallet.article.weight_kg
        val bijstortItemCount = ( bijstortAmount / itemAmount ).toInt
        xnge.put(TransferItemCountBetweenPallets,bijstortItemCount)
        val bijstortScoopAmount = bijstortAmount - (bijstortItemCount * srcPallet.article.weight_kg)
        xnge.put(BijstortScoopAmount,bijstortScoopAmount)
      }) ~> //
      TransferItemsBetweenPallets
  }
  
  object BijstortScoopAmount {}
  
  val processView = new ProcessSwingView(process);
  
  println("Hello bijstort: Transfer Goal")
  
  val product1 = Product("P1")
  
  val article_P1_10 = Article("A0",product1,10.0);
  val article_P1_20 = Article("A1",product1,20.0);
  
  Pallet("WarehousePallet1",article_P1_10,100)
  Pallet("BijstortPallet1",article_P1_20,50)
    
  val xnge = new Exchange();
  xnge.put(Scanner, Scanner(1))
  xnge.put(PalletCode,"BijstortPallet1")
  xnge.put(Article,article_P1_10)
  xnge.put(BijstortAmount,101.101)

  
  process.process(xnge)
  
  System.out.println("BijstortScoopAmount="+xnge.get(BijstortScoopAmount)
)

  
}  

object BijstortAmount {}



/////////////////////////////////////

object TransferItemCountBetweenPallets {}


class TransferItemsBetweenPallets extends Step {

  override def process(xnge:Exchange) = {
    val srcPallet = xnge.get(ScanAnyPalletWithArticle).asInstanceOf[Pallet]
    val dstPallet = xnge.get(ScanThePalletWithCode).asInstanceOf[Pallet]
    val transferItemCountBetweenPallets = xnge.get(TransferItemCountBetweenPallets).asInstanceOf[Int]
    Pallet.transfer(srcPallet, dstPallet,transferItemCountBetweenPallets)
  }
}

object TransferItemsBetweenPallets extends TransferItemsBetweenPallets {
  
}

/////////////////// ----


// ----

object SrcPallet {}
object DstPallet {}

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


