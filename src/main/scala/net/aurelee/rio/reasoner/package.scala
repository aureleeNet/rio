package net.aurelee.rio

import net.aurelee.rio.core.{Formula, Norm, OutOperator, body, head, mkImpl, mkConjs, cnfFormulaToMultiset, cnf}
import net.aurelee.rio.sat.allMUSes

package object reasoner {
  import sat.globalPicoSATInstance

  final case class RioConfig(operator: OutOperator, throughput: Boolean, constrained: Option[NetOutputFunction], constraints: Seq[Formula]) {
    def pretty: String = s"config<out=${operator.name}, throughput=$throughput, constrained=${constrained.toString}, constraints=${constraints.map(_.pretty).mkString("[", ",", "]")}>"
  }

  final def materialization(norms: Seq[Norm]): Seq[Formula] = {
    norms.map(n => mkImpl(body(n), head(n)))
  }

  final def getDirectlyTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    import sat.consequence
    norms.filter(n => consequence(input, body(n)))
  }

  final def getMinimallyWeaklyTriggeredSets(input: Seq[Formula], cnfOfNegatedBodies: Seq[Seq[Seq[Formula]]]): Seq[Seq[Seq[Formula]]] = {
    val cnfOfInput: Seq[Seq[Formula]] = cnfFormulaToMultiset(cnf(mkConjs(input))) //simp(cnf(mkConjs(input))).conjs.map(_.disjs)
//    println(s"cnfOfInput = ${cnfOfInput.toString()}")
    val negatedBodiesInput = cnfOfNegatedBodies.flatten
    val musInput = cnfOfInput.concat(negatedBodiesInput)
    val muses = allMUSes(musInput)
    muses.map { mus =>
      //  if MUS is completely contained in input A then A is contradictory and everything follows, i.e., return empty MUS.
      if (cnfOfInput.nonEmpty && mus.forall(cnfOfInput.contains)) {
        Seq.empty
      } else mus.intersect(negatedBodiesInput) // Filter from muses only those that come from negatedBodies (might not be distinct from input, though)
    }
  }

  final def maxFamily(outOperator: OutOperator,
                      input: Seq[Formula],
                      norms: Seq[Norm],
                      constraints: Seq[Formula],
                      throughput: Boolean): Seq[Seq[Norm]] = {
    import net.aurelee.rio.sat.consistent
    var normsSets: Seq[Seq[Norm]] = Vector(norms)
    var result: Seq[Seq[Norm]] = Vector.empty
    while(normsSets.nonEmpty) {
      val consistentNorms = normsSets.filter(n => consistent(outOperator.apply(n, input, throughput).concat(constraints)))
      result = result.concat(consistentNorms)
      val inconsistentNorms = normsSets.diff(consistentNorms)
      normsSets = inconsistentNorms.flatMap(subsetsOneSmaller).distinct
      normsSets = normsSets.filterNot(n => result.exists(r => subset(n,r)))
    }
    result
  }

  private[this] def subsetsOneSmaller[A](set: Seq[A]): Seq[Seq[A]] = {
    var result: Seq[Seq[A]] = Vector.empty
    set.foreach { s =>
      result = result :+ set.diff(Seq(s))
    }
    result
  }

  private[this] def subset[A](a: Seq[A], b: Seq[A]): Boolean = a.diff(b).isEmpty

  final def outFamily(maxFamily: Seq[Seq[Norm]],
                      outOperator: OutOperator,
                      input: Seq[Formula],
                      throughput: Boolean): Seq[Seq[Formula]] = maxFamily.map(outOperator.apply(_, input, throughput))

}
