package nl.dgl.ptb.dsl

import java.util.HashMap
import scala.collection.mutable.ListBuffer

trait ProcessListener {
  
    def notifyProcessExchangePut(process:Process,key:Any,value:Any);
    def notifyProcessExchangeRename(process:Process,oldKey:Any,newKey:Any);
    def notifyProcessExchangeRemove(process:Process,key:Any);
}

class Process(val top:Step) extends Step with ExchangeListener {
  
  override def process(xnge:Exchange) = {
    xnge.listeners += this
    top.process(xnge)
    xnge.listeners -= this
  }
  
  //////////////
  
  val listeners: ListBuffer[ProcessListener] = ListBuffer.empty
  
  override def notifyExchangePut(key:Any,value:Any) {
    listeners.foreach(_.notifyProcessExchangePut(this,key, value))
  }
  
  override def notifyExchangeRename(oldKey:Any,newKey:Any) {
    listeners.foreach(_.notifyProcessExchangeRename(this,oldKey, newKey))
  }
  override  def notifyExchangeRemove(key:Any) {
    listeners.foreach(_.notifyProcessExchangeRemove(this,key))
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

trait ExchangeListener {
    def notifyExchangePut(key:Any,value:Any);
    def notifyExchangeRename(oldKey:Any,newKey:Any);
    def notifyExchangeRemove(key:Any);
}
   

class Exchange {
  
  private val properties = new HashMap[Any,Any]
  
  def get(key:Any) = {
     properties.get(key);
  } 
  
  def rename(oldKey:Any,newKey:Any) = {
     listeners.foreach(_.notifyExchangeRename(oldKey,newKey))
     properties.put(newKey,properties.get(oldKey));
     properties.remove(oldKey)
  } 
  
  def put(key:Any,value:Any) = {
    listeners.foreach(_.notifyExchangePut(key,value))
    properties.put(key, value);
  }
  
  def remove(key:Any) = {
    listeners.foreach(_.notifyExchangeRemove(key))
    properties.remove(key);
  }
  
  def containsKey(key:Any) = {
    properties.containsKey(key);
  }
  
  val listeners: ListBuffer[ExchangeListener] = ListBuffer.empty

}
