package nl.dgl.logces

import java.time.Instant
import scala.Vector
import nl.dgl.ptb.dsl.Step
import nl.dgl.ptb.dsl.Exchange

case class Scanner(location: Int) {
  /**
   * Waits a Scan and returns an event with the time and scanned code.
   *
   * Note: the method blocks until the scan-event occurs.
   */
  def scan() = {
    println(this + " will scan a random pallet!");
    val pallet = Pallet.random;
    new ScannerEvent(Instant.now, pallet.code)
  }

}

object Scanner {

  def apply(location: Int) = {
    s(location)
  }

  val s = Vector(new Scanner(0), new Scanner(1), new Scanner(2), new Scanner(3)) // inject

}

case class ScannerEvent(instant: Instant, code: String) {}

// ----

class Scan extends Step {

  /**
   * Sets Scanner.Event using Scanner
   */
  override def process(xnge: Exchange) = {
    val scanner = xnge.get(Scanner).asInstanceOf[Scanner]
    xnge.put(ScannerEvent, scanner.scan())
  }
}

object Scan {

  def apply() = {
    new Scan()
  }

}
