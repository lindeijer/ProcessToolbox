package nl.dgl.bsv.ui.swing

import scala.swing.Frame
import nl.dgl.bsv.BijStortVoorbereiding
import nl.dgl.ptb.ui.swing.ProcessSwingView
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Button
import scala.swing.Label
import scala.swing.ListView
import scala.collection.mutable.ListBuffer
import nl.dgl.ptb.dsl.ExchangeEvent
import nl.dgl.ptb.dsl.StepEvent

class BijStortVoorbereidingView(bsv: BijStortVoorbereiding) extends Frame {

  val processView = new ProcessSwingView(bsv.process)

  processView.processViewListeners += notifyProcessViewChanged

  def notifyProcessViewChanged() = {
    pack()
    validate()
    repaint()
  }

  bsv.process.listeners += processView.notifyStepChanged
  bsv.process.xngeListeners += notifyExchangeChanged

  title = "BijStortVoorbereiding"
  val exchangeEvents: ListBuffer[String] = ListBuffer.empty

  val exchangeView = new ListView[String](exchangeEvents);

  val topBox = new BoxPanel(Orientation.Horizontal) {
    contents += processView.component
    contents += exchangeView
  }

  contents = topBox

  pack()
  centerOnScreen()
  open()

  ///////////////////////////

  def notifyExchangeChanged(xngeEvent: ExchangeEvent): Unit = {
    exchangeEvents += xngeEvent.toString
    exchangeView.listData = exchangeEvents
    exchangeView.ensureIndexIsVisible(exchangeEvents.size)
    pack();
  }

}
