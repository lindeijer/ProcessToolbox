package nl.dgl.ptb.dsl

import java.util.HashMap

class Process(val top:Step) extends Step {
  
  override def process(xnge:Exchange) = {
      top.process(xnge)
  }
}

object Process {
  
  def apply(top:Step) = {
    new Process(top)
  }
  
}

// ------

case class StepConcurrent(a:Step,b:Step) extends Step {
  
  override def process(xnge:Exchange) = {
    a.process(xnge); 
    b.process(xnge);
    // concurrent implies any interleaving allowed, including sequential.
  }
  
}

case class StepSequential(val a:Step,val b:Step) extends Step {
  
  override def process(xnge:Exchange) = {
    a.process(xnge)
    b.process(xnge); 
  }
  
}

class StepChoice(steps:Vector[Step],chooser:String) extends Step {
  
  override def process(xnge:Exchange) = {
    val chosenIndex = xnge.get(chooser).asInstanceOf[Int]
    val chosenStep = steps(chosenIndex)
    chosenStep.process(xnge)
  }
  
}

class StepSplit(in:String,out:String,step: Step) extends Step {

  override def process(xnge:Exchange) = {
    val inputList = xnge.get(in).asInstanceOf[List[Any]]
    val outputMap = new HashMap[Any,Any]
    inputList.foreach(input => {
      xnge.put(in,input)
      step.process(xnge);
      val output = xnge.get(out)
      outputMap.put(input, output)
    })
    xnge.put(out,outputMap)
  }
  
}

abstract class Step  {
    
  def ~> (next :Step) : Step = {
    new StepSequential(this,next);
  }
  
  def &&(next: Step) : StepConcurrent = {
    new StepConcurrent(this,next)    
  }
  
  def process(xnge:Exchange) = {
    println("Step["+ this.getClass.getName + "]")
  }
  
}

class StepFunction(f : Exchange=>Any) extends Step {
  
  override def process(xnge:Exchange) = {
    f.apply(xnge)
  }
  
}

object Step extends StepFunction(null) {
  
  def apply(f : Exchange => Any):Step = {
    return new StepFunction(f); 
  }
  
}

class Exchange extends HashMap[Any,Any] {
  
  def rename(oldKey:Any,newKey:Any) {
     this.put(newKey,this.get(oldKey));
     this.remove(oldKey)
     
  }  
  
}
