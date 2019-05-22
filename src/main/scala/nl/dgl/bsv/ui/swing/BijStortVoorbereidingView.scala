package nl.dgl.bsv.ui.swing

import scala.swing.Frame
import nl.dgl.bsv.BijStortVoorbereiding
import nl.dgl.ptb.ui.swing.ProcessSwingView
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Button
import scala.swing.Label
import nl.dgl.ptb.dsl.ProcessListener
import scala.swing.ListView
import scala.collection.mutable.ListBuffer

class BijStortVoorbereidingView(bsv:BijStortVoorbereiding)  extends Frame with ProcessListener {
  
  val processView = new ProcessSwingView(bsv.process)
  
  bsv.process.listeners += this;
  
  title = "BijStortVoorbereiding"
  val xxx : ListBuffer[String] = ListBuffer.empty
  
  val exchangeView = new ListView[String](xxx);
  
  
  val topBox = new BoxPanel(Orientation.Horizontal) {
    contents += processView.component // left
   // contents += new Label("AA") // left
    contents += exchangeView  // right  
   // contents += new Label("BB") // left

  }

  contents = topBox
  
  pack()
  centerOnScreen()
  open()
 
 
  //////////////////////////////
  
  //sexchangeView.peer.getModel()
  
  def notifyProcessExchangePut(process: nl.dgl.ptb.dsl.Process,key: Any,value: Any): Unit = {
    notifyProcessExchangeChanged("put: "+key+"="+value)
  }
  
  def notifyProcessExchangeRemove(process: nl.dgl.ptb.dsl.Process,key: Any): Unit = {
    notifyProcessExchangeChanged("remove: "+key)
  }
  
  def notifyProcessExchangeRename(process: nl.dgl.ptb.dsl.Process,oldKey: Any,newKey: Any): Unit = {
    notifyProcessExchangeChanged("rename: "+oldKey+"->"+newKey)  
  }
  
  def notifyProcessExchangeChanged(reason:String): Unit = {
    xxx += reason;
    exchangeView.listData = xxx
    exchangeView.ensureIndexIsVisible(xxx.size)
  }
  

}

