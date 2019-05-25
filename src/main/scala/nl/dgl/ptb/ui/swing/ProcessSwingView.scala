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

class ProcessSwingView(process: Process) extends Component {

  val graph = new mxGraph();

  val parent = graph.getDefaultParent();

  graph.getModel().beginUpdate();

  val layout = new mxHierarchicalLayout(graph);

  val step2view: HashMap[Step, Object] = HashMap.empty

  try {
    viewStep(process.top, parent, lhs = true) // lhs=left-hand-side is irrelevant at the top.
    layout.execute(graph.getDefaultParent());
  } finally {
    graph.getModel().endUpdate();
  }

  private val graphComponent = new mxGraphComponent(graph);

  val component = Component.wrap(graphComponent)

  /////////////////////////////////////////////

  def viewStep(step: Step, parent: Any, lhs: Boolean): Object = { // mxCell
    step match {
      case StepSequential(before, after) => {
        val vBefore = viewStep(before, parent, lhs = true);
        val vAfter = viewStep(after, parent, lhs = false);
        graph.insertEdge(parent, null, "~>", vBefore, vAfter);
        if (lhs) return vAfter else return vBefore
      }
      case _ => {
        val vertex = graph.insertVertex(parent, null, step.getClass.getName, 0, 0, 80, 30, "fillColor=green"); // 20, 20, 80,30 // x,y,w,h
        step2view.put(step, vertex)
        return vertex
      }
    }
  }

  def notifyProcessStepChanged(stepEvent: StepEvent): Unit = {
    step2view.get(stepEvent.step).foreach(view => {
      graph.setCellStyle("fillColor=blue", Array(view))
    })
  }

}
