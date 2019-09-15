package nl.dgl.logces

import java.time.Instant
import scala.Vector
import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.Exchange

case class Scanner(location: Int) {
  /**
   * Waits a Scan and returns an event with the time and scanned code.
   *
   * Note: the method blocks until the scan-event occurs.
   */
  def scan() = {
    val pallet = Pallet.random; // ok, only pallets for now
    new ScannerEvent(Instant.now, pallet.id)
  }

}

object Scanner {

  def apply(location: Int) = {
    s(location)
  }

  val s = Vector(new Scanner(0), new Scanner(1), new Scanner(2), new Scanner(3)) // inject

}

case class ScannerEvent(instant: Instant, code: String) {}

