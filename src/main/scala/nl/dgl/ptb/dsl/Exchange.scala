package nl.dgl.ptb.dsl

import scala.collection.mutable.ListBuffer
import java.time.Instant
import scala.collection.mutable.HashMap
import scala.concurrent.Promise
import java.util.concurrent.atomic.AtomicInteger

class ExchangeEvent {}
case class ExchangePut(key: Any, value: Any) extends ExchangeEvent {}
case class ExchangeRemove(key: Any) extends ExchangeEvent {}
case class ExchangeRename(oldKey: Any, newKey: Any) extends ExchangeEvent {}

trait Exchange {
  def get[T](key: String): T
  def rename(oldKey: Any, newKey: Any)
  def put(key: String, value: Any)
  def remove(key: Any)
  def containsKey(key: Any): Boolean
  val listeners: ListBuffer[ExchangeEvent => Unit] = ListBuffer.empty

  // def getLocal[T](key: String): Option[T]

  def step(nextStepIndex: Int): Exchange
  def getStepIndex(): Int
  def getIsStepFinished(): Boolean
  def setStepIsFinished()
}

class ExchangeHashMap extends Exchange {

  override def step(nextStepIndex: Int): Exchange = {
    return this;
  }

  def getStepIndex(): Int = 0

  def getIsStepFinished() = false
  def setStepIsFinished() = {}

  private val properties = new HashMap[Any, Any]

  def get[T](key: String): T = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.get(key).getOrElse(null).asInstanceOf[T];
  }

  def rename(oldKey: Any, newKey: Any) = {
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
    listeners.foreach(_.apply(ExchangeRename(oldKey, newKey)))
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
    listeners.foreach(_.apply(ExchangePut(key, value)))
    properties.put(key, value);
  }

  def remove(key: Any) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    if (properties.contains(key)) {
      listeners.foreach(_.apply(ExchangeRemove(key)))
      properties.remove(key);
    }
  }

  def containsKey(key: Any) = {
    if (key == null) {
      throw new IllegalArgumentException("The key may not be null.");
    }
    properties.contains(key);
  }

  // ----

  private val stash = new HashMap[Any, Any]

  def stash_get[T](key: Any): T = {
    if (key == null) {
      throw new IllegalArgumentException("The stash-key may not be null.");
    }
    stash.get(key).asInstanceOf[T];
  }

  def stash_rename(oldKey: Any, newKey: Any) = {
    stash.put(newKey, properties.get(oldKey));
    stash.remove(oldKey)
  }

  def stash_put(key: Any, value: Any) = {
    stash.put(key, value);
  }

  def stash_remove(key: Any) = {
    stash.remove(key);
  }

  def stash_containsKey(key: Any) = {
    stash.contains(key);
  }

}