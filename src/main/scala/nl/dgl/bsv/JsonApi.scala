package nl.dgl.bsv

import cats.effect._, org.http4s._, org.http4s.dsl.io._, scala.concurrent.ExecutionContext.Implicits.global

import cats.implicits._
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.Router

import scala.swing.Frame
import nl.dgl.ptb.dsl.ActionSequential
import nl.dgl.ptb.dsl.ActionSplit
import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.Process
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.ActionSelect
import jdk.nashorn.internal.parser.JSONParser
import org.http4s.server.middleware.Jsonp
import org.apache.tinkerpop.shaded.jackson.core.JsonParser
import scala.concurrent.Future

object JsonApi {

  def getLinkedActions(action: Action, parent: Action, predecessor: Action): List[(Action, Action, Action)] = {
    val thisAction = List((action, parent, predecessor));
    action match {
      case Process(top, index) => {
        return thisAction ++ getLinkedActions(top, action, predecessor);
      }
      case ActionSequential(before, after, index) => {
        val beforeActionList = getLinkedActions(before, action, null)
        val lastBeforeAction = beforeActionList.reverse.head._1
        val afterActionList = getLinkedActions(after, action, lastBeforeAction)
        return beforeActionList ++ afterActionList
      }
      case ActionSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, actionToSplit, index) => {
        return thisAction ++ getLinkedActions(actionToSplit, action, null)
      }
      case ActionSelect(filter, index) => {
        return thisAction
      }
      case Step(func, index) => {
        return thisAction
      }
      case _ => {
        println("??? action=" + action)
        return thisAction;
      }
    }

  }

  def getProcessJsonApi(process: Process): String = {
    val actionList = getLinkedActions(process, null, null);
    val actionsAsJsonApi = actionList.map(x => getActionJsonApi(x._1, x._2, x._3)).mkString(",")
    val result = s"""{ "data" : [ $actionsAsJsonApi ] } """;
    val actions = actionList.map(_._1)
    val whatever = Future(JsonApiAsync.pushStateOfActions(actions))
    return result;
  }

  def getActionJsonApi(action: Action, parent: Action, predecessor: Action): String = {
    val parentRel = {
      if (parent == null) { "" } else {
        val parentId = parent.index
        s""","parent": { "data" : { "id":"$parentId" , "type":"action" } }"""
      }
    }
    val predecessorRel = {
      if (predecessor == null) { "" } else {
        val predecessorId = predecessor.index
        s""","predecessor": { "data" : { "id":"$predecessorId" , "type":"action" } }"""
      }
    }
    return s"""{
      "id":"${action.index}", "type":"action", "attributes":{ "label":"${getActionLabel(action)}" , "state":"CREATED" }
      $parentRel 
      $predecessorRel
    }"""
  }

  def getActionLabel(action: Action): String = {
    val id = action.index
    action match {
      case ActionSequential(before, after, index) => return "seq(" + id + ")"
      case ActionSplit(splitListKey, splitItemKey, splitItemResultKey, splitResultsKey, actionToSplit, index) => return "split(" + id + ")"
      case Process(top, index) => return "proc(" + id + ")"
      case Step(func, index) => return "step(" + id + ")"
      case ActionSelect(func, index) => return "select(" + id + ")"
      case _ => {
        println("!!!!!!!!!!!!!!!!! HUH ???? action=" + _);
        return action.toString()
      }
    }
  }

}