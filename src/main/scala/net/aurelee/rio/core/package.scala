package net.aurelee.rio

import leo.datastructures.TPTP

import scala.annotation.{tailrec, unused}

package object core {
  trait Pretty {
    def pretty: String
  }

  type Formula = PLFormula
  type Head = Formula
  type Body = Formula
  type Norm = (Body, Head)
  /** Multiset-based clause type.
   * contract: all formulas are literals
   */
  type Clause = Seq[Formula]
  /**
   * Multiset-based CNF type.
   * To be interpreted as conjunction of disjunctive clauses.
   */
  type CNF = Seq[Clause]
  /**
   * Multiset-based CNF type.
   * To be interpreted as disjunction of conjunctive clauses.
   */
  @unused type DNF = Seq[Clause]

  @unused final def prettyNorm(norm: Norm): String = s"[${norm._1.pretty}, ${norm._2.pretty}]"

  @inline final def head(norm: Norm): Head = norm._2
  @inline final def heads(norms: Iterable[Norm]): Seq[Head] = norms.map(head).toSeq
  @inline final def body(norm: Norm): Body = norm._1
  @inline @unused final def bodies(norms: Seq[Norm]): Seq[Body] = norms.map(body)

  final def isUnitClause(clause: Formula): Boolean = {
    clause match {
      case PLNeg(PLProp(_)) | PLProp(_) => true
      case _ => false
    }
  }
  final def getUnitAtom(unit: Formula): Formula = {
    (unit: @unchecked) match {
      case PLNeg(prop@PLProp(_)) => prop
      case prop@PLProp(_) => prop
    }
  }
  final def getUnitPolarityAsFormula(unit: Formula): Formula = {
    (unit: @unchecked) match {
      case PLNeg(PLProp(_)) => PLBottom
      case PLProp(_) => PLTop
    }
  }

  final def unitComplement(unit: Formula): Formula = {
    (unit: @unchecked) match {
      case PLNeg(inner@PLProp(_)) => inner
      case PLProp(_) => mkNeg(unit)
    }
  }

  @inline final def mkConj(left: Formula, right: Formula): Formula = mkConjs(Seq(left, right))
  @inline final def mkConjs(formulas: Iterable[Formula]): Formula = if (formulas.isEmpty) PLTop else formulas.reduce(PLConj)
  @inline final def mkDisj(left: Formula, right: Formula): Formula = mkDisjs(Seq(left, right))
  @inline final def mkDisjs(formulas: Iterable[Formula]): Formula = if (formulas.isEmpty) PLBottom else formulas.reduce(PLDisj)
  @inline final def mkImpl(antecedent: Formula, consequent: Formula): Formula = mkDisj(mkNeg(antecedent), consequent)
  @inline final def mkNeg(formula: Formula): Formula = PLNeg(formula)
  @inline final def mkProp(name: String): Formula = PLProp(name)

  final def nnf(formula: Formula): Formula = {
    formula match {
      case PLNeg(body) => body match {
        case PLNeg(body) => nnf(body)
        case PLDisj(left, right) => mkConj(nnf(mkNeg(left)), nnf(mkNeg(right)))
        case PLConj(left, right) => mkDisj(nnf(mkNeg(left)), nnf(mkNeg(right)))
        case PLTop => PLBottom
        case PLBottom => PLTop
        case _ => mkNeg(body)
      }
      case PLDisj(left, right) => PLDisj(nnf(left), nnf(right))
      case PLConj(left, right) => PLConj(nnf(left), nnf(right))
      case _ => formula
    }
  }
  @inline final def cnf(formula: Formula): Formula = cnf0(nnf(formula))
  private[this] final def cnf0(formula: Formula): Formula = {
    formula match {
      case PLDisj(PLConj(ll, lr), r) =>
        cnf0(mkConj(mkDisj(ll, r), mkDisj(lr, r)))
      case PLDisj(l, PLConj(rl, rr)) =>
        cnf0(mkConj(mkDisj(l, rl), mkDisj(l, rr)))
      case PLDisj(left, right) =>
        val leftCNF = cnf0(left)
        val rightCNF = cnf0(right)
        (leftCNF, rightCNF) match {
          case (PLConj(_, _), _) => cnf0(mkDisj(leftCNF, rightCNF))
          case (_, PLConj(_, _)) => cnf0(mkDisj(leftCNF, rightCNF))
          case _ => mkDisj(leftCNF, rightCNF)
        }
      case PLConj(left, right) => mkConj(cnf0(left), cnf0(right))
      case _ => formula
    }
  }

  @inline @unused final def dnf(formula: Formula): Formula = dnf0(nnf(formula))
  private[this] final def dnf0(formula: Formula): Formula = {
    formula match {
      case PLConj(PLDisj(ll, lr), r) =>
        dnf0(mkDisj(mkConj(ll, r), mkConj(lr, r)))
      case PLConj(l, PLDisj(rl, rr)) =>
        dnf0(mkDisj(mkConj(l, rl), mkConj(l, rr)))
      case PLConj(left, right) =>
        val leftDNF = dnf0(left)
        val rightDNF = dnf0(right)
        (leftDNF, rightDNF) match {
          case (PLDisj(_, _), _) => dnf0(mkConj(leftDNF, rightDNF))
          case (_, PLDisj(_, _)) => dnf0(mkConj(leftDNF, rightDNF))
          case _ => mkConj(leftDNF, rightDNF)
        }
      case PLDisj(left, right) => mkDisj(dnf0(left), dnf0(right))
      case _ => formula
    }
  }

  @tailrec final def simp(formula: Formula): Formula = {
    val result = simp0(formula)
    if (result == formula) formula
    else simp(result)
  }
  private[this] final def simp0(formula: Formula): Formula = {
    formula match {
      case PLNeg(body) => simp0(body) match {
        case PLNeg(body) => body
        case PLTop => PLBottom
        case PLBottom => PLTop
        case simpBody => mkNeg(simpBody)
      }
      case PLDisj(left, right) =>
        val simpLeft = simp0(left)
        val simpRight = simp0(right)
        (simpLeft, simpRight) match {
          case (PLTop, _) => PLTop
          case (_, PLTop) => PLTop
          case (PLBottom, _) => simpRight
          case (_, PLBottom) => simpLeft
          case (x, y) if x == y => x
          case (x, PLNeg(y)) if x == y => PLTop
          case (PLNeg(x), y) if x == y => PLTop
          case _ => mkDisj(simpLeft, simpRight)
        }
      case PLConj(left, right) =>
        val simpLeft = simp0(left)
        val simpRight = simp0(right)
        (simpLeft, simpRight) match {
          case (PLTop, _) => simpRight
          case (_, PLTop) => simpLeft
          case (PLBottom, _) => PLBottom
          case (_, PLBottom) => PLBottom
          case (x, y) if x == y => x
          case (x, PLNeg(y)) if x == y => PLBottom
          case (PLNeg(x), y) if x == y => PLBottom
          case _ => mkConj(simpLeft, simpRight)
        }
      case _ => formula
    }
  }
  final def clauseSimp(clause: Clause): Clause = {
    val distinctLiterals = clause.distinct
    // Simp by indirection for now
    val simped = simp(mkDisjs(distinctLiterals))
    val result = if (simped == PLBottom) Seq.empty else simped.disjs
    result
  }
  final def isTrivial(clause: Clause): Boolean = {
    clause.contains(PLTop)
  }

  final def isFalse(clause: Clause): Boolean = {
    clause.isEmpty
  }

  final def interreduce(formulas: Seq[Formula]): Seq[Formula] = {
    if (formulas.isEmpty) formulas
    else {
      val simpSet = formulas.map(simp)
      val cnfSimp = cnf(mkConjs(simpSet))
      val cnfSimpAsClauses: Set[Clause] = cnfFormulaToMultiset(cnfSimp).toSet
      val unprocessed: collection.mutable.Queue[Clause] = collection.mutable.Queue.from(cnfSimpAsClauses)
      val processed: collection.mutable.Set[Clause] = collection.mutable.Set.empty
      while (unprocessed.nonEmpty) {
        val next0:Clause = unprocessed.dequeue()
        val next: Clause = clauseSimp(next0)
        if (isTrivial(next)) { /* skip */ }
        else if (isFalse(next)) { unprocessed.clear; processed.clear; processed += next }
        else {
          if (processed.exists(p => p.size < next.size && p.forall(lit => next.contains(lit)))) {
            /* skip next clause, it's subsumbed by some clause in processed */
          } else {
            // check for backwards subsumption, i.e., if next subsumes some processed clauses
            val subsumed = processed.filter(p => p.size > next.size && next.forall(lit => p.contains(lit)))
            if (subsumed.nonEmpty) {
              subsumed.foreach { cl => processed.remove(cl) }
            }
            // apply subsumption resolution, if possible
            val (removeClauses, newClauses) = subsumptionRes(processed, next)
            processed += next // will maybe be removed by `removeClauses` again
            unprocessed.enqueueAll(newClauses.map(clauseSimp).filterNot(isTrivial))
            processed --= removeClauses
          }
        }
      }
      processed.map(mkDisjs).toSeq
    }
  }

  // Sometimes I really hate Scala ... with doesn't this exist in the collections library?
  private final def seqWithout[A](list: Seq[A], element: A): Seq[A] = seqWithout0(list, element, Seq.empty)
  @tailrec @inline private final def seqWithout0[A](list: Seq[A], element: A, acc: Seq[A]):Seq[A] = {
    if (list.isEmpty) acc
    else {
      val hd = list.head
      val tail = list.tail
      if (hd == element) acc ++ tail
      else seqWithout0(tail, element, acc :+ hd)
    }
  }

  final def subsumptionRes(processed: collection.mutable.Set[Clause], clause: Clause): (Seq[Clause], Seq[Clause]) = {
    val newClauses: collection.mutable.Set[Clause] = collection.mutable.Set.empty
    val removeClauses: collection.mutable.Set[Clause] = collection.mutable.Set.empty
    var done = false
    processed.foreach { otherClause =>
      if (!done) {
        val (potentialSubsumer, potentialSubsumed) = if (otherClause.size <= clause.size) (otherClause, clause) else (clause, otherClause)
        val litCombinations = potentialSubsumer.to(LazyList).flatMap(x => potentialSubsumed.map((x, _)))
        val maybeMatch = litCombinations.find { case (l, r) =>
          l == unitComplement(r) && seqWithout(potentialSubsumer, l).diff(seqWithout(potentialSubsumed, r)).isEmpty
        }
        maybeMatch match {
          case None => // Nothing
          case Some((_, r)) =>
            val newClause: Clause = seqWithout(potentialSubsumed, r)
            if (potentialSubsumed == clause) { // next taken clause is subsumed, remove potentially subsumed clauses by it
              // (will still be subsumed later...)
              done = true
              newClauses.clear()
              removeClauses.clear()
            }
            newClauses += newClause
            removeClauses += potentialSubsumed
        }
      }
    }
    (removeClauses.toSeq, newClauses.toSeq)
  }

  final def cnfFormulaToMultiset(cnf: Formula): CNF = {
    import scala.collection.mutable
    val result: mutable.ListBuffer[Seq[Formula]] = mutable.ListBuffer.empty
    val clauses: Seq[Formula] = simp(cnf).conjs
    clauses.foreach {
      case PLTop => /* skip */
      case PLBottom => result.append(Seq.empty)
      case cl => result.append(cl.disjs)
    }
    result.toSeq
  }

  final def interpretNorm(formula: TPTP.TFF.Formula): Norm = {
    import TPTP.TFF.{NonclassicalPolyaryFormula, NonclassicalLongOperator}
    formula match {
      case NonclassicalPolyaryFormula(NonclassicalLongOperator("$$norm", Seq()), Seq(body, head)) =>
        val left = interpretTFFFormula(body)
        val right = interpretTFFFormula(head)
        (left, right)
      case _ => throw new SemanticsException(s"Norms are expressions of form '{$$$$norm} @ (l,r)' where 'l' and 'r' are " +
        s"formulas, but '${formula.pretty}' was given.")
    }
  }

  final def interpretTFFFormula(formula: TPTP.TFF.Formula): Formula = {
    import TPTP.TFF
    import TFF.{AtomicFormula, UnaryFormula, BinaryFormula}
    formula match {
      case AtomicFormula("$true", Seq()) => PLTop
      case AtomicFormula("$false", Seq()) => PLBottom
      case AtomicFormula(f, Seq()) => mkProp(f)
      case UnaryFormula(_, body) => mkNeg(interpretTFFFormula(body))
      case BinaryFormula(TFF.&, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkConj(left0, right0)
      case BinaryFormula(TFF.~&, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkNeg(mkConj(left0, right0))
      case BinaryFormula(TFF.|, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkDisj(left0, right0)
      case BinaryFormula(TFF.~|, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkNeg(mkDisj(left0, right0))
      case BinaryFormula(TFF.Impl, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkImpl(left0, right0)
      case BinaryFormula(TFF.<=, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkImpl(right0, left0)
      case BinaryFormula(TFF.<=>, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkConj(mkImpl(left0, right0), mkImpl(right0, left0))
      case BinaryFormula(TFF.<~>, left, right) =>
        val left0 = interpretTFFFormula(left)
        val right0 = interpretTFFFormula(right)
        mkNeg(mkConj(mkImpl(left0, right0), mkImpl(right0, left0)))
      case _ => throw new SemanticsException(s"Only the propositional fragment of THF is supported, but '${formula.pretty}' was given.")
    }
  }

  final def interpretTFFTermAsFormula(term: TPTP.TFF.Term): Formula = {
    import leo.datastructures.TPTP.TFF
    term match {
      case TFF.AtomicTerm(f, Seq()) => mkProp(f)
      case TFF.FormulaTerm(formula) => interpretTFFFormula(formula)
      case _ => throw new SemanticsException(s"Term '${term.pretty}' cannot be interpreted as formula or it lies outside of the propositional fragment of TFF.")
    }
  }
}
