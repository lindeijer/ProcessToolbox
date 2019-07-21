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

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.swing.BoxPanel
import scala.swing.Orientation

/**
 * A view upon the process state, where each step has a state.
 */
class ProcessStateView(topProcess: Process) extends BoxPanel(Orientation.Vertical) {

  val graph = new mxGraph() {
    override def convertValueToString(x: Any): String = {
      if (x.isInstanceOf[mxCell]) {
        val cell = x.asInstanceOf[mxCell]
        if (cell.getValue.isInstanceOf[Step]) {
          val step = cell.getValue.asInstanceOf[Step]
          return step.getClass.getSimpleName + "@" + step.index
        }
      }
      return super.convertValueToString(x);
    }
  }

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
    // println("added=" + added)
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
      case StepSelect(_, index) => newStepSelectViewContents(step.asInstanceOf[StepSelect[_]])
      case _                    => new Label("[[" + step.toString() + "]]")
    }

  }

  import scala.concurrent.duration._
  import scala.util.{ Try, Success, Failure }

  def newStepSelectViewContents(stepSelect: StepSelect[_]): Component = {
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

  val vertexes: ListBuffer[mxCell] = ListBuffer.empty;

  try {
    viewStep(topProcess.top, uberParent, isBefore = true) // before is irrelevant
    layout.execute(graph.getDefaultParent());
  } finally {
    graph.getModel().endUpdate();
  }

  val theMxGraphComponent = new mxGraphComponent(graph);
  val graphView = new FlowPanel
  graphView.contents += Component.wrap(theMxGraphComponent);

  contents += graphView
  contents += stepView

  /////////////////////////////////////////////

  def viewStep(step: Step, vParent: Any, isBefore: Boolean): Object = { // mxCell
    step match {
      case StepSequential(before, after, index) => {
        // println("ProcessSwingView.viewStep: StepSequential; isBefore=" + isBefore)
        val vBefore = viewStep(before, vParent, isBefore = true);
        val vAfter = viewStep(after, vParent, isBefore = false);
        graph.insertEdge(vParent, null, "~>", vBefore, vAfter);
        if (isBefore) return vAfter else return vBefore
      }
      case StepFunction(f, index) => {
        // println("ProcessSwingView.viewStep: StepFunction; isBefore=" + isBefore)
        val vFunc = graph.insertVertex(vParent, f.getClass.getSimpleName, step, 0, 0, 80, 30, "fillColor=green");
        vertexes += vFunc.asInstanceOf[mxCell]
        return vFunc
      }
      case Process(top, index) => {
        // println("ProcessSwingView.viewStep: Process; isBefore=" + isBefore)
        val vProcess = graph.insertVertex(vParent, "Process@" + step.index, step, 0, 0, 80, 30, "fillColor=green");
        vertexes += vProcess.asInstanceOf[mxCell]
        viewStep(top, vProcess, isBefore)
        return vProcess
      }
      case StepSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, stepToSplit, index) => {
        // println("ProcessSwingView.viewStep: StepSplit; splitListKey=" + splitListKey + ",isBefore=" + isBefore)
        val vStepSplit = graph.insertVertex(vParent, "Split@" + step.index, step, 0, 0, 80, 30, "fillColor=green");
        vertexes += vStepSplit.asInstanceOf[mxCell]
        val vStepToSplit = viewStep(stepToSplit, vStepSplit, isBefore)
        step.asInstanceOf[StepSplit].items2StepFuture onComplete {
          case Success(items2Step) => {
            graph.getModel().beginUpdate();
            graph.cellsRemoved(Array(vStepToSplit))
            for (stepForItem <- items2Step.values) {
              viewStep(stepForItem, vStepSplit, isBefore)
            }
            layout.execute(graph.getDefaultParent());
            graph.getModel().endUpdate();
          }
          case Failure(t) => { println("Failure:" + t) }
        }
        return vStepSplit
      }
      case StepSelect(filter, index) => {
        // println("ProcessSwingView.viewStep: StepSelect; filter=" + filter + ",isBefore=" + isBefore)
        val vStepSelect = graph.insertVertex(vParent, "Select@" + step.index, step, 0, 0, 80, 30, "fillColor=green");
        vertexes += vStepSelect.asInstanceOf[mxCell]
        return vStepSelect
      }
      case x => {
        // println("ProcessSwingView.viewStep: DEFAULT;  step=" + step + ",isBefore=" + isBefore)
        val vertex = graph.insertVertex(vParent, step.getClass.getName, step, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        vertexes += vertex.asInstanceOf[mxCell]
        return vertex;
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
    vertexes.filter(_.getValue.equals(step)).foreach(vertex => {
      // println("step=" + step + ",vertex=" + vertex)
      graph.setCellStyle(style, Array(vertex))
    })
  }

}
