package nl.dgl.ptb.ui.swing

import nl.dgl.ptb.dsl.Process
import nl.dgl.ptb.dsl.StepSequential
import nl.dgl.ptb.dsl.Step
import com.mxgraph.view.mxGraph
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import nl.dgl.ptb.dsl.StepEvent
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.mxgraph.model.mxCell
import nl.dgl.ptb.dsl.StepStarted
import nl.dgl.ptb.dsl.StepFinished
import nl.dgl.ptb.dsl.StepFunction
import nl.dgl.ptb.dsl.StepSplit
import nl.dgl.ptb.dsl.StepSelect
import com.mxgraph.util.mxEventSource
import com.mxgraph.util.mxEventObject
import com.mxgraph.util.mxEvent
import java.util.Collection
import scala.concurrent.Await
import scala.swing.event.SelectionChanged
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.ComboBox

/**
 * A view upon the process state, where each step has a state.
 */
class ProcessStateView(topProcess: Process) extends Component {

  val graph = new mxGraph();

  val processViewListeners: ListBuffer[() => Unit] = ListBuffer.empty

  object myMxGraphSelectionListener extends mxEventSource.mxIEventListener {
    def invoke(sender: Object, evt: mxEventObject) {
      val addedActuallyRemoved = evt.getProperty("added").asInstanceOf[Collection[_]];
      val removedActuallyAdded = evt.getProperty("removed").asInstanceOf[Collection[_]];
      // selectionChanged(model, added, removed);
      myMxGraphSelectionAdded(removedActuallyAdded)
    }
  }

  val stepView = new FlowPanel

  def setStepViewContents(content: Component) {
    stepView.contents += content;
  }

  setStepViewContents(new Label("Select a Step"))

  def myMxGraphSelectionAdded(added: Collection[_]) {
    println("added=" + added)
    if (added == null || added.size() != 1) {
      return ;
    }
    stepView.contents.clear()
    val addedCells = added.toArray // (0) //
    val addedCell = addedCells(0).asInstanceOf[mxCell]
    val addedCellValue = addedCell.getValue
    addedCellValue match {
      case valueIsStep: Step => {
        setStepViewContents(newStepViewContents(valueIsStep.asInstanceOf[Step]))
      }
      case valueIsString: String => {
        setStepViewContents(new Label("[" + valueIsString + "]"))
      }
    }
    processViewListeners.foreach(_.apply())
  }

  def newStepViewContents(step: Step): Component = {
    step match {
      case StepSelect(_) => newStepSelectViewContents(step.asInstanceOf[StepSelect])
      case _             => new Label("[[" + step.toString() + "]]")
    }

  }

  import scala.concurrent.duration._
  import scala.util.{ Try, Success, Failure }

  def newStepSelectViewContents(stepSelect: StepSelect): Component = {
    Try(Await.result(stepSelect.candidatesFuture, 1 second)) match {
      case Success(candidates) => {
        val stepSelectComboBox = new ComboBox(candidates);
        listenTo(stepSelectComboBox.selection) // when do we stop listening to this cb?
        reactions += {
          case SelectionChanged(`stepSelectComboBox`) =>
            stepSelect.selectionPromise.success(stepSelectComboBox.selection.item)
          // stepSelectComboBox.enabled = false the split-step is not stateless yet
        }
        return stepSelectComboBox
      }
      case Failure(failure) => { return new Label(failure.getMessage) }
      case _                => { return new Label("Very Strange") }
    }

  }

  graph.getSelectionModel.addListener(mxEvent.CHANGE, myMxGraphSelectionListener)

  val uberParent = graph.getDefaultParent();

  graph.getModel().beginUpdate();

  val layout = new mxHierarchicalLayout(graph);

  val step2view: HashMap[Step, Object] = HashMap.empty

  try {
    viewStep(topProcess.top, uberParent, isBefore = true) // before is irrelevant
    layout.execute(graph.getDefaultParent());
  } finally {
    graph.getModel().endUpdate();
  }

  private val graphComponent = new mxGraphComponent(graph)

  val component = new FlowPanel
  component.contents += Component.wrap(graphComponent)
  component.contents += stepView

  /////////////////////////////////////////////

  def viewStep(step: Step, vParent: Any, isBefore: Boolean): Object = { // mxCell
    step match {
      case StepSequential(before, after) => {
        println("ProcessSwingView.viewStep: StepSequential; isBefore=" + isBefore)
        val vBefore = viewStep(before, vParent, isBefore = true);
        val vAfter = viewStep(after, vParent, isBefore = false);
        graph.insertEdge(vParent, null, "~>", vBefore, vAfter);
        if (isBefore) return vAfter else return vBefore
      }
      case stepFunction: StepFunction => {
        println("ProcessSwingView.viewStep: StepFunction; isBefore=" + isBefore)
        val vStep = graph.insertVertex(vParent, null, stepFunction.f.getClass.getSimpleName, 0, 0, 80, 30, "fillColor=green");
        step2view.put(step, vStep)
        return vStep
      }
      case process: Process => {
        println("ProcessSwingView.viewStep: Process; isBefore=" + isBefore)
        val vProcess = graph.insertVertex(vParent, null, "Process", 0, 0, 80, 30, "fillColor=green");
        val vResult = viewStep(process.top, vProcess, isBefore)
        step2view.put(step, vProcess)
        return vResult
      }
      case StepSplit(a, stepKey, c, d, stepToSplit) => {
        println("ProcessSwingView.viewStep: StepSplit; stepKey=" + stepKey + ",isBefore=" + isBefore)
        val vStepSplit = graph.insertVertex(vParent, null, "Split", 0, 0, 80, 30, "fillColor=green");
        val vResult = viewStep(stepToSplit, vStepSplit, isBefore)
        step2view.put(step, vStepSplit)
        return vResult
      }
      case StepSelect(filter) => {
        println("ProcessSwingView.viewStep: StepSelect; filter=" + filter + ",isBefore=" + isBefore)
        val vStepSelect = graph.insertVertex(vParent, "Select", step, 0, 0, 80, 30, "fillColor=green");
        step2view.put(step, vStepSelect)
        return vStepSelect
      }

      case x => {
        println("ProcessSwingView.viewStep: DEFAULT;  step=" + step + ",isBefore=" + isBefore)
        val vertex = graph.insertVertex(vParent, null, step.getClass.getName, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        step2view.put(step, vertex)
        return vertex
      }
    }
  }

  def notifyStepChanged(stepEvent: StepEvent): Unit = {
    stepEvent match {
      case StepStarted(step, instant) => {
        setStepViewStyle(step, "fillColor=yellow")
      }
      case StepFinished(step, instant) => {
        setStepViewStyle(step, "fillColor=blue")
      }
    }
  }

  def setStepViewStyle(step: Step, style: String) {
    step2view.get(step).foreach(view => {
      graph.setCellStyle(style, Array(view))
    })
  }

}
