package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger

class ExchangeEvent(xngeId: Int) {}
case class ExchangePut(xngeId: Int, key: Any, value: Any) extends ExchangeEvent(xngeId) {}
case class ExchangeRemove(xngeId: Int, key: Any) extends ExchangeEvent(xngeId) {}
case class ExchangeRename(xngeId: Int, oldKey: Any, newKey: Any) extends ExchangeEvent(xngeId) {}

trait Exchange {
  def get[T](key: String): Option[T]
  def rename(oldKey: String, newKey: String)
  def put(key: String, value: Any)
  def remove(key: String)
  def containsKey(key: String): Boolean
  val listeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

  def step(nextStepIndex: Int): Exchange
  def split(nextStepIndex: Int): Exchange
  def getStepIndex(): Int
  def getIsStepFinished(): Boolean
  def setStepIsFinished()
}

class ExchangeHashMap extends Exchange {

  override def step(nextStepIndex: Int): Exchange = {
    return this;
  }

  override def split(nextStepIndex: Int): Exchange = {
    val clone = new ExchangeHashMap();
    this.properties.foreach(x => {
      clone.put(x._1, x._2);
    })
    this.stash.foreach(x => {
      clone.stash.put(x._1, x._2);
    })
    clone.listeners += notifyCloneChanged
    return clone.step(nextStepIndex);
  }

  def notifyCloneChanged(xngeEvent: ExchangeEvent) {
    listeners.foreach(_.apply(xngeEvent))
  }

  def getStepIndex(): Int = 0

  def getIsStepFinished() = false
  def setStepIsFinished() = {}

  private val properties = new HashMap[String, Any]

  def get[T](key: String): Option[T] = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.get(key).asInstanceOf[Option[T]]
  }

  def rename(oldKey: String, newKey: String) = {
    if (oldKey == null) {
      if (newKey == null) {
        throw new IllegalArgumentException("Neither old-key nor new-key may be null, and both are null.");
      } else {
        throw new IllegalArgumentException("The old-key may not be null. Attempted with new-key=" + newKey);
      }
    }
    if (newKey == null) {
      throw new IllegalArgumentException("The new-key not be null. Allempted with old-key=" + oldKey);
    }
    listeners.foreach(_.apply(ExchangeRename(this.hashCode(), oldKey, newKey)))
    properties.put(newKey, properties.get(oldKey));
    properties.remove(oldKey)
  }

  def put(key: String, value: Any) = {
    if (key == null) {
      if (value == null) {
        throw new IllegalArgumentException("Neither value nor key may be null, and both are null.");
      } else {
        throw new IllegalArgumentException("The key may not be null. Allempted use with value=" + value);
      }
    }
    if (value == null) {
      throw new IllegalArgumentException("The value may not be null. key=" + key);
    }
    listeners.foreach(_.apply(ExchangePut(this.hashCode(), key, value)))
    properties.put(key, value);
  }

  def remove(key: String) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    if (properties.contains(key)) {
      listeners.foreach(_.apply(ExchangeRemove(this.hashCode(), key)))
      properties.remove(key);
    }
  }

  def containsKey(key: String) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.contains(key);
  }

  // ----

  private val stash = new HashMap[String, Any]

  def stash_get[T](key: String): T = {
    if (key == null) {
      throw new IllegalArgumentException("The stash-key may not be null.");
    }
    stash.get(key).asInstanceOf[T];
  }

  def stash_rename(oldKey: String, newKey: String) = {
    stash.put(newKey, properties.get(oldKey));
    stash.remove(oldKey)
  }

  def stash_put(key: String, value: Any) = {
    stash.put(key, value);
  }

  def stash_remove(key: String) = {
    stash.remove(key);
  }

  def stash_containsKey(key: String) = {
    stash.contains(key);
  }

}