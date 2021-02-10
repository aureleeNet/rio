package net.aurelee.rio.sat

import org.scalatest.funsuite.AnyFunSuite

class PicoSatTest extends AnyFunSuite {
  def loadSatProblem(c: PicoSAT): Unit = {
    c.addClause(List(-1, -3, -2))
    c.addClause(List(-2, -3, -1))
    c.addClause(List(3))
  }

  test("Load PicoSAT") {
    val picosat = PicoSAT.apply(true)
    println(s"PicoSAT version: ${picosat.version()}")
    println("Test sccess: PicoSAT loaded.")
  }

  test("Get PicoSAT context") {
    val context = PicoSAT(true)
    assert(context.state == PicoSAT.Unknown, "State of fresh context not unknown.")
    println("Test success: Got PicoSAT context.")
  }

  test("Satisfiable problem.") {
    val context = PicoSAT(true)
    val clause = List(1, -1)
    context.addClause(clause)
    assert(context.numAddedClauses == 1)
    assert(context.solve() == PicoSAT.SAT)
    assert(context.state == PicoSAT.SAT)
    println("Test success: Satisfiable.")
  }

  test("Trivial SAT problem.") {
    val context = PicoSAT(true)
//    val clause = List(1, -1)
//    context.addClause(clause)
    assert(context.numAddedClauses == 0)
    assert(context.solve() == PicoSAT.SAT)
    assert(context.state == PicoSAT.SAT)
    println("Test success: Satisfiable.")
  }

  test("Trivial UNSAT problem.") {
    val context = PicoSAT(true)
    val clause = List()
    context.addClause(clause)
    assert(context.numAddedClauses == 1)
    assert(context.solve() == PicoSAT.UNSAT)
    assert(context.state == PicoSAT.UNSAT)
    println("Test success: Unsatisfiable.")
  }

  test("Unsatisfiable problem.") {
    val context = PicoSAT(true)
    context.addClause(1)
    context.addClause(-1)
    assert(context.numAddedClauses == 2)
    assert(context.solve() == PicoSAT.UNSAT)
    assert(context.state == PicoSAT.UNSAT)
    println("Test success: Unsatisfiable problem.")
  }

  test("Reset works.") {
    val context = PicoSAT(true)
    loadSatProblem(context)
    assert(context.numAddedClauses == 3)
    assert(context.solve() == PicoSAT.SAT)
    context.reset()
    assert(context.state == PicoSAT.Unknown)
    assert(context.numAddedClauses == 0)
    println("Test success: Reset works.")
  }

  test("Fresh variables.") {
    val context = PicoSAT(true)
    context.addClause(1)
    context.addClause(-1)
    assert(context.freshVariable == 2)
    context.adjust(10)
    assert(context.freshVariable == 11)
    println("Test success: Fresh variables.")
  }

  test("Adjustment functions callable.") {
    val context = PicoSAT(true)
    loadSatProblem(context)
    context.setGlobalDefaultPhase(3)
    context.setDefaultPhase(1, -3)
    context.setMoreImportant(1)
    context.setLessImportant(3)
    assert(context.solve() == PicoSAT.SAT)
    context.resetPhases()
    context.resetScores()
    context.removeLearnedClauses(50)
    println("Test success: Adjustment functions callable.")
  }

  test("Statistics.") {
    val context = PicoSAT(true)
    assert(context.numAddedClauses == 0)
    assert(context.numVariables == 0)

    loadSatProblem(context)

    assert(context.numAddedClauses == 3)
    assert(context.numVariables == 3)
    assert(context.timeSpendSolving == 0)

    // TODO: Since the result is in seconds this fails.
    context.solve()
    assert(context.timeSpendSolving >= 0)

    println("Test success: Statistics.")
  }

  test("Get Assignment") {
    val context = PicoSAT(true)
    loadSatProblem(context)

    assert(context.getAssignment(1).isEmpty)
    assert(context.getAssignment(2).isEmpty)
    assert(context.getAssignment(3).isEmpty)

    context.solve()

    assert(context.getAssignment(1).contains(false))
    assert(context.getAssignment(2).contains(false))
    assert(context.getAssignment(3).contains(true))
    assertResult(Some(true))(context.getAssignmentToplevel(3))

    println("Test success: Get Assignment.")
  }

  test("Inconsistency") {
    val context = PicoSAT(true)
    loadSatProblem(context)

    context.addClause()

    assertResult(true)(context.inconsistent)
    println("Test success: Inconsitency.")
  }

  test("Assumptions") {
    val context = PicoSAT(true)
    loadSatProblem(context)

    context.assume(1)
    assertResult(context.solve())(PicoSAT.SAT)

    context.assume(1)
    context.assume(2)
    assertResult(context.solve())(PicoSAT.UNSAT)
    assert(context.failedAssumption(1))
    assert(context.failedAssumption(2))

    assertResult(context.failedAssumptions)(List(1,2))

    println("Test success: Assumptions.")
  }

  test("Changed") {
    val context = PicoSAT(true)
    loadSatProblem(context)
    context.solve()
    context.solve()
    assert(!context.changed)
    println("Test success: Changed.")
  }

  test("Cores") {
    val context = PicoSAT(true)
    context.addClause(1, 3)
    context.addClause(2, 3)
    context.addClause(-3)
    context.addClause(-1)
    context.solve()

    assert(context.coreLiteral(1))
    assert(!context.coreLiteral(2))
    assert(context.usedLiteral(1))

    assert(context.coreClause(0))
    assert(!context.coreClause(1))

    println("Test success: Cores.")
  }
}
