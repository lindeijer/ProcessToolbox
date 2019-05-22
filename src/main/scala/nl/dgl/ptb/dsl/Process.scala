package nl.dgl.ptb.dsl

import java.util.HashMap
import scala.collection.mutable.ListBuffer

class ProcessEvent {}
case class ProcessExchangeChanged(process:Process,xngeEvent:ExchangeEvent) extends ProcessEvent {}

class Process(val top:Step) extends Step {
  
  override def process(xnge:Exchange) = {
    xnge.listeners += notifyExchangeChanged
    top.process(xnge)
    xnge.listeners -= notifyExchangeChanged
  }
  
  ////////////////////////
  
  def notifyExchangeChanged(xngeEvent: ExchangeEvent) = {
    listeners.foreach(_.apply(ProcessExchangeChanged(this,xngeEvent)))
  }
  
  val listeners: ListBuffer[ProcessEvent=>Unit] = ListBuffer.empty
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

class ExchangeEvent {}
  case class ExchangePut(key:Any,value:Any) extends ExchangeEvent {}
  case class ExchangeRemove(key:Any) extends ExchangeEvent {}
  case class ExchangeRename(oldKey:Any,newKey:Any) extends ExchangeEvent {}
  

class Exchange {
  
  private val properties = new HashMap[Any,Any]
  
  def get(key:Any) = {
     properties.get(key);
  } 
  
  def rename(oldKey:Any,newKey:Any) = {
     listeners.foreach(_.apply(ExchangeRename(oldKey,newKey)))
     properties.put(newKey,properties.get(oldKey));
     properties.remove(oldKey)
  } 
  
  def put(key:Any,value:Any) = {
    listeners.foreach(_.apply(ExchangePut(key,value)))
    properties.put(key, value);
  }
  
  def remove(key:Any) = {
    listeners.foreach(_.apply(ExchangeRemove(key)))
    properties.remove(key);
  }
  
  def containsKey(key:Any) = {
    properties.containsKey(key);
  }
   
  val listeners: ListBuffer[ExchangeEvent=>Unit] = ListBuffer.empty

}

