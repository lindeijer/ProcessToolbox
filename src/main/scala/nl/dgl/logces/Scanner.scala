package nl.dgl.logces

import java.time.Instant
import scala.Vector
import nl.dgl.ptb.dsl.Action
import nl.dgl.ptb.dsl.Exchange
import nl.dgl.ptb.dsl.Location
import scala.collection.mutable.ListBuffer

case class Scanner(location: LogisticLocation) {
  /**
   * Waits a Scan and returns an event with the time and scanned code.
   *
   * Note: the method blocks until the scan-event occurs.
   */
  def scan() = {
    val pallet = Pallet.random; // ok, only pallets for now
    new ScannerEvent(Instant.now, pallet.id)
  }

  Scanner.scanners += this

}

object Scanner {

  def apply(location: LogisticLocation) = {
    scanners.find(_.location.id == location.id).getOrElse(new Scanner(location))
  }

  private val scanners = ListBuffer.empty[Scanner]

  new Scanner(LogisticLocation("0")) //
  new Scanner(LogisticLocation("1")) //
  new Scanner(LogisticLocation("2")) //
  new Scanner(LogisticLocation("3")) //

}

case class ScannerEvent(instant: Instant, code: String) {}

