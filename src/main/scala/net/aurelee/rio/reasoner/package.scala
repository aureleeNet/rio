package net.aurelee.rio

import net.aurelee.rio.core.{CNF, Formula, Norm, OutOperator, body, cnf, cnfFormulaToMultiset, head, mkConjs, mkImpl, prettyNorm}
import net.aurelee.rio.sat.{allMUSes, consequence, consistent}

package object reasoner {
  import sat.globalPicoSATInstance

  final case class RioConfig(operator: OutOperator, throughput: Boolean,
                             constrained: Option[NetOutputFunction], constraints: Seq[Formula],
                             preferenceRelation: Option[Seq[Seq[String]]]) {
    def pretty: String = s"config<out=${operator.name}, throughput=$throughput, constrained=${constrained.toString}, " +
      s"constraints=${constraints.map(_.pretty).mkString("[", ",", "]")}, " +
      s"preference=${preferenceRelation.fold("<none>")(_.map(_.mkString("[", ",", "]")).mkString("[", ",", "]"))}>"
  }

  trait PreferenceRelation[A] {
    def greaterThanOrEqual(left: A, right: A): Boolean

    def strictlyGreaterThan(left: A, right: A): Boolean = greaterThanOrEqual(left, right) && !greaterThanOrEqual(right, left)
  }

  object PreferenceRelation {
    final def fromSeqs[A](input: Seq[Seq[A]]): PreferenceRelation[A] = {
      import collection.mutable
      val normRanking0: mutable.Map[A, Int] = mutable.Map.empty
      var idx = 0
      input.foreach { normset =>
        normset.foreach { norm => normRanking0.addOne(norm, idx) }
        idx = idx + 1
      }
      new PreferenceRelation[A] {
        val normRanking: Map[A, Int] = normRanking0.toMap
        override def greaterThanOrEqual(left: A, right: A): Boolean = {
          (normRanking.get(left), normRanking.get(right)) match {
            case (Some(leftRanking), Some(rightRanking)) => leftRanking <= rightRanking
            case _ => false
          }
        }

        override def strictlyGreaterThan(left: A, right: A): Boolean = {
          (normRanking.get(left), normRanking.get(right)) match {
            case (Some(leftRanking), Some(rightRanking)) => leftRanking < rightRanking
            case _ => false
          }
        }

        override def toString: String = input.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
      }
    }

    final def brassLifting[A](pref: PreferenceRelation[A]): PreferenceRelation[Seq[A]] = new PreferenceRelation[Seq[A]] {
      override def greaterThanOrEqual(left: Seq[A], right: Seq[A]): Boolean = {
        val leftMinusRight = left diff right
        val rightMinusLeft = right diff left
        rightMinusLeft.forall { elem1 =>
          leftMinusRight.exists( elem2 => pref.greaterThanOrEqual(elem2, elem1))
        }
      }
    }

    final def getMaximals[A](pref: PreferenceRelation[A], set: Seq[A]): Seq[A] = {
//      import collection.mutable
//      val result: mutable.Buffer[A] = mutable.ArrayBuffer.empty
//      set.foreach { elem =>
//        set.forall(pref.greaterThanOrEqual(elem, _))
//      }
//      result.toSeq

//      set.filter(x => set.forall(pref.greaterThanOrEqual(x, _)))
      set.filter(x => !set.exists(y => pref.strictlyGreaterThan(y, x)))
    }
  }

  final def materialization(norms: Seq[Norm]): Seq[Formula] = {
    norms.map(n => mkImpl(body(n), head(n)))
  }

  final def getDirectlyTriggeredNorms(input: Seq[Formula], norms: Seq[Norm]): Seq[Norm] = {
    norms.filter(n => consequence(input, body(n)))
  }

  final def getMinimallyWeaklyTriggeredSets(input: Seq[Formula], cnfOfNegatedBodies: Seq[CNF]): Seq[CNF] = {
    val cnfOfInput: CNF = cnfFormulaToMultiset(cnf(mkConjs(input)))
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
    var normsSets: Seq[Seq[Norm]] = Vector(norms)
    var result: Seq[Seq[Norm]] = Vector.empty
    println(s"toCheck = ${normsSets.map(x => x.map(prettyNorm).mkString("{", ",", "}")).mkString(",\n")}")
    while(normsSets.nonEmpty) {
      val consistentNorms = normsSets.filter(n => consistent(outOperator.apply(n, input, throughput).concat(constraints)))
      println(s"consistentNorms = ${consistentNorms.map(x => x.map(prettyNorm).mkString("{", ",", "}")).mkString(",\n")}")
      result = result.concat(consistentNorms)
      val inconsistentNorms = normsSets.diff(consistentNorms)
      println(s"inconsistentNorms = ${inconsistentNorms.map(x => x.map(prettyNorm).mkString("{", ",", "}")).mkString(",\n")}")
      normsSets = inconsistentNorms.flatMap(subsetsOneSmaller).distinct
      normsSets = normsSets.filterNot(n => result.exists(r => subset(n,r)))
      println(s"toCheck = ${normsSets.map(x => x.map(prettyNorm).mkString("{", ",", "}")).mkString(",\n")}")
    }
    result
  }

  final def maxFamilyWithNames(outOperator: OutOperator,
                      input: Map[String, Formula],
                      norms: Map[String, Norm],
                      constraints: Seq[Formula],
                      throughput: Boolean): Seq[Map[String, Norm]] = {
    var normsSets: Seq[Map[String, Norm]] = Vector(norms)
    var result: Seq[Map[String, Norm]] = Vector.empty
//    println(s"toCheck = ${normsSets.map(x => x.keys.mkString("{", ",", "}")).mkString(",\n")}")
    while (normsSets.nonEmpty) {
      val consistentNorms = normsSets.filter(n => consistent(outOperator.apply(n.values.toSeq, input.values.toSeq, throughput).concat(constraints)))
//      println(s"consistentNorms = ${consistentNorms.map(x => x.keys.mkString("{", ",", "}")).mkString(",\n")}")
      result = result.concat(consistentNorms)
      val inconsistentNorms = normsSets.diff(consistentNorms)
//      println(s"inconsistentNorms = ${inconsistentNorms.map(x => x.keys.mkString("{", ",", "}")).mkString(",\n")}")
      normsSets = inconsistentNorms.flatMap(subMapsOneSmaller).distinct
      normsSets = normsSets.filterNot(n => result.exists(r => subset(n.values.toSeq, r.values.toSeq)))
//      println(s"toCheck = ${normsSets.map(x => x.keys.mkString("{", ",", "}")).mkString(",\n")}")
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

  private[this] def subMapsOneSmaller[A,B](map: Map[A,B]): Seq[Map[A,B]] = {
    var result: Seq[Map[A,B]] = Vector.empty
    map.foreach { case (a,_) =>
      result = result :+ map.removed(a)
    }
    result
  }

  private[this] def subset[A](a: Seq[A], b: Seq[A]): Boolean = a.diff(b).isEmpty

  final def outFamily(maxFamily: Seq[Seq[Norm]],
                      outOperator: OutOperator,
                      input: Seq[Formula],
                      throughput: Boolean): Seq[Seq[Formula]] = maxFamily.map(outOperator.apply(_, input, throughput))

}
