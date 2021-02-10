package net.aurelee.rio

import leo.datastructures.TPTP

import scala.annotation.tailrec

package object core {
  trait Pretty {
    def pretty: String
  }

  type Formula = PLFormula
  type Norm = (Formula, Formula)

  final def prettyNorm(norm: Norm): String = s"[${norm._1.pretty} , ${norm._2.pretty}]"

  @inline final def head(norm: Norm): Formula = norm._2
  @inline final def heads(norms: Seq[Norm]): Seq[Formula] = norms.map(head)
  @inline final def body(norm: Norm): Formula = norm._1
  @inline final def bodies(norms: Seq[Norm]): Seq[Formula] = norms.map(body)

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
  @inline final def dnf(formula: Formula): Formula = dnf0(nnf(formula))
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
          case _ => mkConj(simpLeft, simpRight)
        }
      case _ => formula
    }
  }

  final def interreduce(formulas: Seq[Formula]): Seq[Formula] = {
    import net.aurelee.rio.sat.consequence
    val simpSet = formulas.map(simp)
    val cnfSimp = cnf(mkConjs(simpSet)).conjs
    val intermediate = cnfSimp
    // TODO: unit propagation
    var result: Seq[Formula] = Seq.empty
    intermediate.foreach { f =>
      if (!consequence(result, f)) {
        result = result.filterNot(x => consequence(Seq(f), x))
        result = result :+ f
      }
    }
    result
  }

  final def interpretNorm(formula: TPTP.THF.Formula): Norm = {
    import TPTP.THF.Tuple
    formula match {
      case Tuple(elements) if elements.size == 2 =>
        val left = interpretFormula(elements.head)
        val right = interpretFormula(elements(1))
        (left, right)
      case _ => throw new SemanticsException(s"Norms have to be tuples with two elements, but '${formula.pretty}' was given.")
    }
  }
  final def interpretFormula(formula: TPTP.THF.Formula): Formula = {
    import TPTP.THF
    import THF.{FunctionTerm, UnaryFormula, BinaryFormula}
    formula match {
      case FunctionTerm("$true", Seq()) => PLTop
      case FunctionTerm("$false", Seq()) => PLBottom
      case FunctionTerm(f, Seq()) => mkProp(f)
      case UnaryFormula(_, body) => mkNeg(interpretFormula(body))
      case BinaryFormula(THF.&, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkConj(left0, right0)
      case BinaryFormula(THF.~&, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkNeg(mkConj(left0, right0))
      case BinaryFormula(THF.|, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkDisj(left0, right0)
      case BinaryFormula(THF.~|, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkNeg(mkDisj(left0, right0))
      case BinaryFormula(THF.Impl, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkImpl(left0, right0)
      case BinaryFormula(THF.<=, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkImpl(right0, left0)
      case BinaryFormula(THF.<=>, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkConj(mkImpl(left0, right0), mkImpl(right0, left0))
      case BinaryFormula(THF.<~>, left, right) =>
        val left0 = interpretFormula(left)
        val right0 = interpretFormula(right)
        mkNeg(mkConj(mkImpl(left0, right0), mkImpl(right0, left0)))
      case _ => throw new SemanticsException(s"Only the propositional fragment of THF is supported, but '${formula.pretty}' was given.")
    }
  }
}
