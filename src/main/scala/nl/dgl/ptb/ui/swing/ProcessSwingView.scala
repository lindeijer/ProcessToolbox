package nl.dgl.ptb.ui.swing

import scala.swing._
import nl.dgl.ptb.dsl.Process
import nl.dgl.ptb.dsl.StepSequential
import nl.dgl.ptb.dsl.StepConcurrent
import nl.dgl.ptb.dsl.StepSplit
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
import nl.dgl.ptb.dsl.StepSplit
import nl.dgl.ptb.dsl.StepSplit

class ProcessSwingView(topProcess: Process) extends Component {

  val graph = new mxGraph();

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

  private val graphComponent = new mxGraphComponent(graph);

  val component = Component.wrap(graphComponent)

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
