package nl.dgl.ptb.ui.swing

import scala.swing._
import nl.dgl.ptb.dsl.Process
import nl.dgl.ptb.dsl.StepSequential
import nl.dgl.ptb.dsl.StepConcurrent
import nl.dgl.ptb.dsl.Step
import com.mxgraph.view.mxGraph
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.layout.mxParallelEdgeLayout
import com.mxgraph.layout.mxStackLayout
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import nl.dgl.ptb.dsl.StepEvent
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.mxgraph.model.mxCell
import nl.dgl.ptb.dsl.StepStarted
import nl.dgl.ptb.dsl.StepFinished
import nl.dgl.ptb.dsl.StepFunction

class ProcessSwingView(topProcess: Process) extends Component {

  val graph = new mxGraph();

  val parent = graph.getDefaultParent();

  graph.getModel().beginUpdate();

  val layout = new mxHierarchicalLayout(graph);

  val step2view: HashMap[Step, Object] = HashMap.empty

  try {
    viewStep(topProcess.top, parent, isBefore = true) // before is irrelevant
    layout.execute(graph.getDefaultParent());
  } finally {
    graph.getModel().endUpdate();
  }

  private val graphComponent = new mxGraphComponent(graph);

  val component = Component.wrap(graphComponent)

  /////////////////////////////////////////////

  def viewStep(step: Step, parent: Any, isBefore: Boolean): Object = { // mxCell
    step match {
      case StepSequential(before, after) => {
        val vBefore = viewStep(before, parent, isBefore = true);
        val vAfter = viewStep(after, parent, isBefore = false);
        graph.insertEdge(parent, null, "~>", vBefore, vAfter);
        if (isBefore) return vAfter else return vBefore
      }
      case stepFunction: StepFunction => {
        val vStep = graph.insertVertex(parent, null, stepFunction.f.getClass.getName, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        step2view.put(step, vStep)
        return vStep
      }
      case process: Process => {
        println("process=" + process + ",process.top=" + process.top + ",isBefore=" + isBefore)
        val vProcess = graph.insertVertex(parent, null, process.getClass.getName, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        val vResult = viewStep(process.top, vProcess, isBefore) // if you are on the left, return the right and vice-versa
        step2view.put(step, vProcess)
        return vResult
      }

      case _ => {
        val vertex = graph.insertVertex(parent, null, step.getClass.getName, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
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
