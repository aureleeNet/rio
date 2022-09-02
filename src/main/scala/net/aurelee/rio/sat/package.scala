package net.aurelee.rio

import net.aurelee.rio.core.{CNF, Formula}
import net.aurelee.rio.core.{PLNeg, PLProp}
import net.aurelee.rio.core.{cnfFormulaToMultiset, mkConjs, mkImpl, mkNeg}

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

  final def cnfToDimacsClauses(multisetCNF: CNF): (Seq[Seq[Int]], Map[String, Int]) = {
    //    println(s"cnfToDimacsClause of cnf = ${cnf.pretty}")
    val translatedClauses: mutable.ListBuffer[Seq[Int]] = mutable.ListBuffer.empty
    val namingMap: mutable.Map[String, Int] = mutable.Map.empty

    @inline def translateLiteral(literalName: String): Int = { // translate names to integers 0 ... k
      namingMap.get(literalName) match {
        case Some(value) => value
        case None =>
          val newValue = namingMap.size
          namingMap.addOne(literalName -> newValue)
          newValue
      }
    }

    multisetCNF.foreach { clause => // iterate over clauses
      val translatedLits = clause.map { literal =>
        (literal: @unchecked) match {
          case PLNeg(PLProp(name)) => // Negated because in dimacs negative literals are negative
            val value = translateLiteral(name)
            -(value + 1) // +1 because in dimacs we cannot use 0 as literal name
          case PLProp(name) =>
            val value = translateLiteral(name)
            value + 1 // see above
        }
      }
      translatedClauses.append(translatedLits)
    }
    (translatedClauses.toSeq, namingMap.toMap)
  }

  final def cnfToDimacsProblemString(multisetCNF: CNF): (String, Map[String, Int]) = {
    //    println(s"cnfToDimacsProblem cnf = ${cnf.pretty}")
    val sb: mutable.StringBuilder = new mutable.StringBuilder()
    val (dimacsClauses, symbolnameMap) = cnfToDimacsClauses(multisetCNF)
    val varCount = symbolnameMap.size
    val clauseCount = dimacsClauses.size
    sb.append(s"p cnf $varCount $clauseCount\n")
    dimacsClauses foreach { clause =>
      sb.append(clause.mkString(" "))
      sb.append(s" 0\n")
    }
    (sb.init.toString /*drop last newline*/, symbolnameMap)
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
    import net.aurelee.rio.core.cnf
    val clausified = cnf(formula)
    val multisetCNF = cnfFormulaToMultiset(clausified)
//    println(s"[PicoSAT] Check ${simplified.pretty}")
    val (picoSatInput, _) = cnfToDimacsClauses(multisetCNF)
    solver.reset()
    picoSatInput.foreach(solver.addClause)
    solver.solve()
  }

  final def allMUSes(multisetCNF: CNF)(implicit solver: PicoSAT): Seq[CNF] = {
    // translate to Dimacs
    val (dimacs, symbolMap) = cnfToDimacsProblemString(multisetCNF)
    val reverseMap = symbolMap.map(_.swap) // works because symbolMap is injective
    // run MUS enumeration tool
    val mustResult = runMUST(dimacs)
    val muses = MUSesFromDimacs(mustResult)
    // translate result file to Seq[Formula]
    val result: Seq[CNF] = muses.map { mus =>
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
