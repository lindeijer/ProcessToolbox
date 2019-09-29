package nl.dgl.logces

import java.time.Instant
import scala.Vector
import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.Exchange
import scala.util.Random

trait Scale {

  val location: Int
  def weigh(targetAmount: Double, marginPercent: Double): ScaleEvent

}

object Scale {

  def apply(location: Int) = {
    s(location)
  }

  val s = Vector(ScaleRandom(0), ScaleRandom(1), ScaleRandom(2), ScaleRandom(3))

}

case class ScaleEvent(instant: Instant, amount: Double) {}

///////////// implementations /////////////

case class ScaleRandom(location: Int) extends Scale {

  val rndm = new Random

  def weigh(targetAmount: Double, marginPercent: Double): ScaleEvent = {
    println(this + " will weigh an amount within the margin!");
    Thread.sleep(1 * 1000)
    val marginPos = rndm.nextDouble() * (marginPercent / 100) * targetAmount
    val weighedAmount = (if (math.random < 0.5) targetAmount + marginPos else targetAmount - marginPos)
    ScaleEvent(Instant.now, weighedAmount)
  }

}
