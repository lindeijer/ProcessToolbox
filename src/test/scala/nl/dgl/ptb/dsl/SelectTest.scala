package nl.dgl.ptb.dsl

import org.scalactic.source.Position.apply
import org.scalatest.AsyncFlatSpec

class ActionSelectSpec extends AsyncFlatSpec {

  behavior of "ActionSelect"

  it should "select candidates during start invocation" in {
    // in this test select candidates who's name start with 'da'.
    val selectCandidatesWherePrefix = Select(Candidates Where Candidates.Prefix)

    val candidates = Candidates.candidates(null)
    val xnge = new ExchangeHashMap()
    xnge.put(Candidates.Prefix, "da")
    val xngeFuture = selectCandidatesWherePrefix.start(xnge);
    val candidatesWithPrefix = selectCandidatesWherePrefix.candidatesFuture.value.get.get
    assert(candidatesWithPrefix.size < candidates.size)
  }

  it should "eventually complete the future exchange with the selected candidate" in {

    val candidates = Candidates.candidates(null);
    val candidate1 = candidates(1)

    val selectCandidatesWherePrefix = Select(Candidates Where Candidates.Prefix)
    val xnge = new ExchangeHashMap()
    xnge.put(Candidates.Prefix, "da")
    val result = selectCandidatesWherePrefix.start(xnge) map {
      xnge => assert(xnge.get[Candidate](DSL.Selection).get.eq(candidate1))
    }
    selectCandidatesWherePrefix.selectionPromise.success(candidate1) // select the candidate
    result
  }

}

// ------------

case class Candidate(name: String)

class CandidateSelectFiler(source: SelectSource[Candidate], xngeKey: String) extends SelectFilter[Candidate] with SelectSource[Candidate] {

  def And(xngeKey: String): SelectFilter[Candidate] = {
    return new CandidateSelectFiler(this, xngeKey)
  }

  // with SelectSource

  def Where(xngeKey: String): SelectFilter[Candidate] = {
    return And(xngeKey)
  }

  def candidates(xnge: Exchange): List[Candidate] = {
    val candidates = source.candidates(xnge)
    val prefix = xnge.get[String](xngeKey).get
    return candidates.filter(_.name.startsWith(prefix))
  }

  def getSelectType(): Class[Candidate] = classOf[Candidate]
}

object Candidates extends SelectSource[Candidate] {

  val Prefix = "Prefix";

  def Where(xngeKey: String): SelectFilter[Candidate] = { new CandidateSelectFiler(this, xngeKey) }
  def candidates(xnge: Exchange): List[Candidate] = {
    // this is the root-select-source so there is no xnge-key-value to select by
    // so we want the current list of them all.
    return List(Candidate("david"), Candidate("daniel"), Candidate("frank"))
  }

}

