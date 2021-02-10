package net.aurelee.rio

import net.aurelee.rio.core.{Formula, Norm, OutOperator, bodies, body, head, mkDisjs, mkImpl}

package object reasoner {
  import sat.globalPicoSATInstance

  final case class RioConfig(operator: OutOperator, throughput: Boolean, constrained: Option[NetOutputFunction], constraints: Seq[Formula])

  final def materialization(norms: Seq[Norm]): Seq[Formula] = {
    norms.map(n => mkImpl(body(n), head(n)))
  }

  final def getDirectlyTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    import sat.consequence
    norms.filter(n => consequence(input, body(n)))
  }

  final def getBasicTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    import sat.consequence
    norms.filter { n =>
      val compat = getCompatibleNorms(norms,n)
      val compatBodies = bodies(compat)
      val bigDisjunction = mkDisjs(compatBodies)
      consequence(input, bigDisjunction)
    }
  }

  final def getCompatibleNorms(norms: Seq[Norm], n: Norm): Seq[Norm] = {
    import net.aurelee.rio.sat.consequence
    val h = head(n)
    norms.filter(m => consequence(Seq(head(m)), h))
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
      val setSize = normsSets.head.size
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
