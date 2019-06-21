package nl.dgl.ptb.ui.swing

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ListView
import scala.collection.mutable.ListBuffer
import nl.dgl.ptb.dsl.ExchangeEvent
import nl.dgl.ptb.dsl.Process
import scala.swing.ScrollPane

class ProcessOverView(process: Process) extends BoxPanel(Orientation.Vertical) {

  val processView = new ProcessStateView(process)

  processView.processViewListeners += notifyProcessViewChanged

  def notifyProcessViewChanged() = {
    validate()
    repaint()
  }

  process.listeners += processView.notifyStepChanged
  process.xngeListeners += notifyExchangeChanged

  val exchangeEvents: ListBuffer[String] = ListBuffer.empty

  val exchangeView = new ListView[String](exchangeEvents);

  contents += new ScrollPane(processView.component)
  contents += new ScrollPane(exchangeView)

  ///////////////////////////

  def notifyExchangeChanged(xngeEvent: ExchangeEvent): Unit = {
    exchangeEvents += xngeEvent.toString
    exchangeView.listData = exchangeEvents
    exchangeView.ensureIndexIsVisible(exchangeEvents.size)
    notifyProcessViewChanged();
  }

}
