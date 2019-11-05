package nl.dgl.bsv

import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.ActionState

object JsonApiAsync {

  def pushStateOfActions(actions: List[Action]) = {
    actions.foreach(action => {
      val state = action.getState();
      action.getState() match {
        case ActionState.CREATED => {
          println("CREATED");
        }
        case ActionState.STARTED => {
          println("STARTED");
        }
        case ActionState.FINISHED => {
          println("FINISHED");
        }

      }
      action.getState().equals()
    })
  }

}