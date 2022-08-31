package net.aurelee.rio

import net.aurelee.rio.core.{Formula, PLNeg, PLProp, PLTop, PLBottom, mkConjs, mkDisjs, mkNeg, mkImpl}

import scala.annotation.unused
import scala.collection.mutable
import scala.sys.process.ProcessLogger

package object sat {
  implicit lazy val globalPicoSATInstance: PicoSAT = PicoSAT.get()
  final lazy val musToolFile: String = {
    val must = getClass.getResourceAsStream("/must")
    val target = java.nio.file.Files.createTempFile("riomust", "")
    java.nio.file.Files.copy(must, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    target.toFile.setExecutable(true)
    target.toFile.deleteOnExit()
    target.toAbsolutePath.toString
  }

  final def cnfToDimacsClauses(symbolMap: Option[Map[String, Int]])(cnf: Formula): Seq[Seq[Int]] = {
//    println(s"cnfToDimacsClause of cnf = ${cnf.pretty}")
    val result: mutable.ListBuffer[Seq[Int]] = mutable.ListBuffer.empty
    val clauses = cnf.conjs
    val namingMap = symbolMap match {
      case Some(value) => value
      case None => cnf.symbols.zipWithIndex.toMap
    }
    clauses.foreach {
      case PLTop => /* skip */
      case PLBottom => result.append(Seq.empty)
      case cl =>
        val lits = cl.disjs
        val translatedLits = lits.map { l =>
          (l: @unchecked) match {
            case PLNeg(PLProp(name)) => -(namingMap(name) + 1)
            case PLProp(name) => namingMap(name) + 1
          }
        }
        result.append(translatedLits)
    }
    result.toSeq
  }

  final def cnfToDimacsProblem(symbolMap: Option[Map[String, Int]])(cnf: Formula): String = {
//    println(s"cnfToDimacsProblem cnf = ${cnf.pretty}")
    val sb: mutable.StringBuilder = new mutable.StringBuilder()
    val namingMap = symbolMap match {
      case Some(value) => value
      case None => cnf.symbols.zipWithIndex.toMap
    }
    val dimacsClauses = cnfToDimacsClauses(Some(namingMap))(cnf)
    val varCount = cnf.symbols.size
    val clauseCount = dimacsClauses.size
    sb.append(s"p cnf $varCount $clauseCount\n")
    dimacsClauses foreach { clause =>
      sb.append(clause.mkString(" "))
      sb.append(s" 0\n")
    }
    sb.init.toString // drop last newline
  }


  final def satisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.SAT
  }
  final def unsatisfiable(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    solveInternal(formula)(solver)
    solver.state == PicoSAT.UNSAT
  }
  final def consistent(formulas: Iterable[Formula])(implicit solver: PicoSAT): Boolean = {
    val conjunct = mkConjs(formulas)
    satisfiable(conjunct)(solver)
  }
  final def consequence(assumptions: Iterable[Formula], conjecture: Formula)(implicit solver: PicoSAT): Boolean = {
    val formulas = Iterable.concat(assumptions, Seq(mkNeg(conjecture)))
    unsatisfiable(mkConjs(formulas))(solver)
  }
  final def tautology(formula: Formula)(implicit solver: PicoSAT): Boolean = {
    unsatisfiable(mkNeg(formula))(solver)
  }
  @unused
  final def equivalent(f: Formula, g: Formula)(implicit solver: PicoSAT): Boolean = {
    val impl = mkImpl(f, g)
    val lpmi = mkImpl(g, f)
    tautology(mkConjs(Seq(impl, lpmi)))(solver)
  }
  private[this] final def solveInternal(formula: Formula)(solver: PicoSAT): Unit = {
    import net.aurelee.rio.core.{cnf, simp}
    val clausified = cnf(formula)
    val simplified = simp(clausified)
//    println(s"[PicoSAT] Check ${simplified.pretty}")
    val picoSatInput = cnfToDimacsClauses(None)(simplified)
    solver.reset()
    picoSatInput.foreach(solver.addClause)
    solver.solve()
  }

  final def allMUSes(cnfInput: Seq[Seq[Formula]])(implicit solver: PicoSAT): Seq[Seq[Seq[Formula]]] = {
    // translate to Dimacs
    val asFormula = mkConjs(cnfInput.map(mkDisjs))            // TODO: back-and-forth-translation ugly, pass as real CNF
    val symbolMap = asFormula.symbols.zipWithIndex.toMap // TODO: Do in dimacs translation
    val reverseMap = symbolMap.map(_.swap) // works because symbolMap is injective
    val dimacs = cnfToDimacsProblem(Some(symbolMap))(asFormula)
    // run MUS enumeration tool
    val mustResult = runMUST(dimacs)
    val muses = MUSesFromDimacs(mustResult)
    // translate result file to Seq[Formula]
    val result: Seq[Seq[Seq[Formula]]] = muses.map { mus =>
        mus.map { clause =>
          clause.map { lit =>
          (lit: @unchecked) match {
            case i if i > 0 => PLProp(reverseMap(i - 1))
            case i if i < 0 => PLNeg(PLProp(reverseMap(-i - 1)))
          }
        }
      }
    }
    result
  }
  private[this] final def runMUST(dimacs: String): String = {
    import java.nio.file.Files
    // write dimacs file to temporary file
    val tempDimacsFile = Files.createTempFile("rio", ".cnf")
        tempDimacsFile.toFile.deleteOnExit()
    val filewriter = Files.newBufferedWriter(tempDimacsFile)
    filewriter.write(dimacs)
    filewriter.flush(); filewriter.close()
    // construct output file
    val tempOutputFile = Files.createTempFile("rio", ".musoutput")
        tempOutputFile.toFile.deleteOnExit()
    // call MUST with input, wait for termination
    val process = scala.sys.process.Process.apply(musToolFile, Seq(tempDimacsFile.toAbsolutePath.toString, "-o", tempOutputFile.toAbsolutePath.toString))
    process ! ProcessLogger(_ => (), err => println(err)) // ignore stdout
    // read result file
    val toolOutput = Files.readString(tempOutputFile)
    toolOutput
  }
  private[this] final def MUSesFromDimacs(musDimacs: String): Seq[Seq[Seq[Int]]] = {
//    println(s"MUSesFromDimacs musDimacs=$musDimacs")
    if (musDimacs.isEmpty) Seq.empty
    else {
      val musEntries = musDimacs.split("\n\n").toSeq
      musEntries.map { entry =>
        val lines = entry.split("\n")
        val clauses = lines.tail.tail.toSeq // drop first two lines: "MUS #i" and "p cnf k j"
        clauses.map { clause =>
          // .init drops the trailing 0 in each clause
          // filter(_.nonEmpty) is required because the split may contain empty strings
          val lits = clause.init.trim.split(" ").toSeq.filter(_.nonEmpty)
          if (lits.isEmpty) Seq.empty
          else lits.map(_.toInt)
        }
      }
    }
  }

}
