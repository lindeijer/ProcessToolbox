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
import nl.dgl.ptb.dsl.ProcessEvent
import nl.dgl.ptb.dsl.ProcessExchangeChanged
import nl.dgl.ptb.dsl.ProcessStepChanged
import nl.dgl.ptb.dsl.StepEvent

class BijStortVoorbereidingView(bsv: BijStortVoorbereiding) extends Frame {

  val processView = new ProcessSwingView(bsv.process)

  bsv.process.listeners += notifyProcessChanged

  title = "BijStortVoorbereiding"
  val xxx: ListBuffer[String] = ListBuffer.empty

  val exchangeView = new ListView[String](xxx);

  val topBox = new BoxPanel(Orientation.Horizontal) {
    contents += processView.component
    contents += exchangeView
  }

  contents = topBox

  pack()
  centerOnScreen()
  open()

  ///////////////////////////

  def notifyProcessChanged(processEvent: ProcessEvent) = {
    processEvent match {
      case ProcessExchangeChanged(process, xngeEvent) => notifyProcessExchangeChanged(xngeEvent)
      case ProcessStepChanged(process, stepEvent) => notifyProcessStepChanged(stepEvent)
      case _ => println("dropped processEvent=" + processEvent)
    }

  }

  //////////////////////////////

  def notifyProcessExchangeChanged(xngeEvent: ExchangeEvent): Unit = {
    xxx += xngeEvent.toString
    exchangeView.listData = xxx
    exchangeView.ensureIndexIsVisible(xxx.size)
  }

  def notifyProcessStepChanged(stepEvent: StepEvent): Unit = {
    println("stepEvent=" + stepEvent)
  }

}
