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

class ProcessSwingView(process: Process) extends Frame {

  title = "Hello Process"

  val graph = new mxGraph();

  val parent = graph.getDefaultParent();

  graph.getModel().beginUpdate();
  
  val layout =  new mxHierarchicalLayout(graph);

  try {
   viewStep(process.top,parent)
   layout.execute(graph.getDefaultParent());
  } finally {
      graph.getModel().endUpdate();
  }

  val graphComponent = new mxGraphComponent(graph); 
    
  contents = Component.wrap(graphComponent)
  pack()
  centerOnScreen()
  open()
  
  def viewStep(step:Step,parent:Any) { // mxCell
    step match {
    case StepSequential(a,b) => {
      val v1 = graph.insertVertex(parent, null, a.getClass.getName, 0, 0, 80,30); // 20, 20, 80,30 // x,y,w,h
      val v2 = graph.insertVertex(parent, null, b.getClass.getName,  0, 0,80, 30); // 140, 150,80, 30
      graph.insertEdge(parent, null, "~>", v1, v2);  
      val vSeq = graph.insertVertex(parent, null,null, 0, 0, 0,  0);
      graph.groupCells(vSeq, 1.0,Array(v1,v2)) 
    }
    case _ => {
      graph.insertVertex(parent, null, step.getClass.getName, 0, 0, 80,30);
    }
    // case StepConcurrent(a,b) =>  viewStepConcurrent(a,b)
   } 
       
  }


}

