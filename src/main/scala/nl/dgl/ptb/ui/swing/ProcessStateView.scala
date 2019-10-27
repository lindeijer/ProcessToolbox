package nl.dgl.ptb.ui.swing

import nl.dgl.ptb.dsl.Process
import nl.dgl.ptb.dsl.ActionSequential
import nl.dgl.ptb.dsl.Action
import com.mxgraph.view.mxGraph
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import nl.dgl.ptb.dsl.ActionEvent
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.mxgraph.model.mxCell
import nl.dgl.ptb.dsl.ActionStarted
import nl.dgl.ptb.dsl.ActionFinished
import nl.dgl.ptb.dsl.ActionSplit
import nl.dgl.ptb.dsl.ActionSelect
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
import nl.dgl.ptb.dsl.BasicAction

/**
 * A view upon the process state, where each sub-action has a state.
 */
class ProcessStateView(topProcess: Process) extends BoxPanel(Orientation.Vertical) {

  val graph = new mxGraph() {
    override def convertValueToString(x: Any): String = {
      if (x.isInstanceOf[mxCell]) {
        val cell = x.asInstanceOf[mxCell]
        if (cell.getValue.isInstanceOf[Action]) {
          val action = cell.getValue.asInstanceOf[Action]
          return action.getClass.getSimpleName + "@" + action.index
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

  val actionView = new FlowPanel

  def setActionViewContents(content: Component) {
    actionView.contents += content;
  }

  setActionViewContents(new Label("Select a Step"))

  def myMxGraphSelectionAdded(added: Collection[_]) {
    // println("added=" + added)
    if (added == null || added.size() != 1) {
      return ;
    }
    actionView.contents.clear()
    val addedCells = added.toArray // (0) //
    val addedCell = addedCells(0).asInstanceOf[mxCell]
    val addedCellValue = addedCell.getValue
    addedCellValue match {
      case valueIsAction: Action => {
        setActionViewContents(newActionViewContents(valueIsAction.asInstanceOf[Action]))
      }
      case valueIsString: String => {
        setActionViewContents(new Label("[" + valueIsString + "]"))
      }
    }
    processViewListeners.foreach(_.apply())
  }

  def newActionViewContents(action: Action): Component = {
    action match {
      case ActionSelect(_, index) => newActionSelectViewContents(action.asInstanceOf[ActionSelect[_]])
      case _                      => new Label("[[" + action.toString() + "]]")
    }

  }

  import scala.concurrent.duration._
  import scala.util.{ Try, Success, Failure }

  def newActionSelectViewContents[T](actionSelect: ActionSelect[T]): Component = {
    Try(Await.result(actionSelect.candidatesFuture, 1 second)) match {
      case Success(candidates) => {
        val actionSelectComboBox = new ComboBox(candidates);
        listenTo(actionSelectComboBox.selection) // when do we stop listening to this cb?
        reactions += {
          case SelectionChanged(`actionSelectComboBox`) =>
            actionSelect.selectionPromise.success(actionSelectComboBox.selection.item)
          // actionSelectComboBox.enabled = false the split-action is not stateless yet
        }
        return actionSelectComboBox
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
    viewAction(topProcess.top, uberParent, isBefore = true) // before is irrelevant
    layout.execute(graph.getDefaultParent());
  } finally {
    graph.getModel().endUpdate();
  }

  val theMxGraphComponent = new mxGraphComponent(graph);
  val graphView = new FlowPanel
  graphView.contents += Component.wrap(theMxGraphComponent);

  contents += graphView
  contents += actionView

  /////////////////////////////////////////////

  def insertEdge(vParent: Any, label: String, o: Any, vBefore: Any, vAfter: Any): Object = {
    graph.insertEdge(vParent, label, o, vBefore, vAfter); // emit message to browser
  }

  def insertVertex(vParent: Any, label: String, action: Any, w: Double, h: Double, x: Double, y: Double, style: String): Object = {
    graph.insertVertex(vParent, label, action, w, h, x, y, style); // emit message to browser
  }

  ////////////////////////////////////////////

  def viewAction(action: Action, vParent: Any, isBefore: Boolean): Object = { // mxCell
    action match {
      case ActionSequential(before, after, index) => {
        // println("ProcessSwingView.viewStep: StepSequential; isBefore=" + isBefore)
        val vBefore = viewAction(before, vParent, isBefore = true);
        val vAfter = viewAction(after, vParent, isBefore = false);
        insertEdge(vParent, null, "~>", vBefore, vAfter);
        if (isBefore) return vAfter else return vBefore
      }
      case BasicAction(f, index) => {
        // println("ProcessSwingView.viewStep: StepFunction; isBefore=" + isBefore)
        val vFunc = graph.insertVertex(vParent, f.getClass.getSimpleName, action, 0, 0, 80, 30, "fillColor=green"); // emit
        vertexes += vFunc.asInstanceOf[mxCell]
        return vFunc
      }
      case Process(top, index) => {
        // println("ProcessSwingView.viewStep: Process; isBefore=" + isBefore)
        val vProcess = graph.insertVertex(vParent, "Process@" + action.index, action, 0, 0, 80, 30, "fillColor=green");
        vertexes += vProcess.asInstanceOf[mxCell]
        viewAction(top, vProcess, isBefore)
        return vProcess
      }
      case ActionSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, actionToSplit, index) => {
        // println("ProcessSwingView.viewStep: StepSplit; splitListKey=" + splitListKey + ",isBefore=" + isBefore)
        val vStepSplit = graph.insertVertex(vParent, "Split@" + action.index, action, 0, 0, 80, 30, "fillColor=green");
        vertexes += vStepSplit.asInstanceOf[mxCell]
        val vStepToSplit = viewAction(actionToSplit, vStepSplit, isBefore)
        action.asInstanceOf[ActionSplit].futureSteps andThen {
          case Success(actionsForSplit) => {
            graph.getModel().beginUpdate();
            graph.cellsRemoved(Array(vStepToSplit))
            for (actionSplit <- actionsForSplit) {
              viewAction(actionSplit, vStepSplit, isBefore)
            }
            layout.execute(graph.getDefaultParent());
            graph.getModel().endUpdate();
          }
          case Failure(t) => { println("Failure:" + t) }
        }
        return vStepSplit
      }
      case ActionSelect(filter, index) => {
        // println("ProcessSwingView.viewStep: StepSelect; filter=" + filter + ",isBefore=" + isBefore)
        val vStepSelect = graph.insertVertex(vParent, "Select@" + action.index, action, 0, 0, 80, 30, "fillColor=green");
        vertexes += vStepSelect.asInstanceOf[mxCell]
        return vStepSelect
      }
      case x => {
        // println("ProcessSwingView.viewStep: DEFAULT;  action=" + action + ",isBefore=" + isBefore)
        val vertex = graph.insertVertex(vParent, action.getClass.getName, action, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        vertexes += vertex.asInstanceOf[mxCell]
        return vertex;
      }
    }
  }

  def notifyActionChanged(actionEvent: ActionEvent): Unit = { // emit to browser
    actionEvent match {
      case ActionStarted(action, instant) => {
        setActionViewStyle(action, "fillColor=yellow")
      }
      case ActionFinished(action, instant) => {
        setActionViewStyle(action, "fillColor=blue")
      }
    }
  }

  def setActionViewStyle(action: Action, style: String) {
    vertexes.filter(_.getValue.equals(action)).foreach(vertex => {
      // println("action=" + action + ",vertex=" + vertex)
      graph.setCellStyle(style, Array(vertex))
    })
  }

}
