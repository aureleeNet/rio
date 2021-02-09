package net.aurelee.rio

import leo.datastructures.TPTP
import leo.datastructures.TPTP.AnnotatedFormula
import leo.modules.input.TPTPParser
import net.aurelee.rio.core.{ConstrainedCredulous, ConstrainedSetting, ConstrainedSkeptical, OutOperator, RioConfig}

import scala.io.Source
import java.io.{File, FileNotFoundException, PrintWriter}

object Main {
  final val name: String = "rio"
  final val version: Double = 0.1

  private[this] var inputFileName = ""
  private[this] var outputFileName: Option[String] = None
  private[this] var outOperatorParameter: Option[String] = None
  private[this] var parameterNames: Set[String] = Set.empty

  def main(args: Array[String]): Unit = {
    if (args.contains("--help")) {
      usage(); return
    }
    if (args.contains("--version")) {
      printVersion(); return
    }
    if (args.isEmpty) usage()
    else {
      var infile: Option[Source] = None
      var outfile: Option[PrintWriter] = None
      var error: Boolean = false
      try {
        parseArgs(args.toSeq)
        // Read input
        infile = Some(if (inputFileName == "-") io.Source.stdin else io.Source.fromFile(inputFileName))
        // Parse and select output operator
        val parsedInput = TPTPParser.problem(infile.get)
        val (rioConfig, input) = getSpecFromProblem(parsedInput.formulas) match {
          case (Some(spec), input) => (generateRioConfigFromSpec(spec), input)
          case (None, input) => (generateRioConfigFromCLI(), input)
        }
        // Magic here
        val result: String = ???
        outfile = Some(if (outputFileName.isEmpty) new PrintWriter(System.out) else new PrintWriter(new File(outputFileName.get)))
        outfile.get.println(result)
        outfile.get.flush()
      } catch {
        case e: IllegalArgumentException =>
          println(e.getMessage)
          usage()
          error = true
        case e: FileNotFoundException =>
          println(s"File cannot be found or is not readable/writable: ${e.getMessage}")
          error = true
        case e: TPTPParser.TPTPParseException =>
          println(s"Input file could not be parsed, parse error at ${e.line}:${e.offset}: ${e.getMessage}")
          error = true
        case e: Throwable =>
          println(s"Unexpected error. ${e.getMessage}")
          println("This is considered an implementation error; please report this!")
          error = true
      } finally {
        infile.foreach(_.close())
        outfile.foreach(_.close())
      }
      if (error) System.exit(1)
    }
  }

  private[this] final def getSpecFromProblem(formulas: Seq[TPTP.AnnotatedFormula]): (Option[TPTP.AnnotatedFormula], Seq[TPTP.AnnotatedFormula]) = {
    val (specs, otherFormulas) = formulas.partition(_.role == "logic")
    if (specs.isEmpty) (None, otherFormulas)
    else if (specs.size == 1) (Some(specs.head), otherFormulas)
    else {
      println(s"% Warning: More than one logic specifications contained in input problem; only the first is considered " +
        s"and the remaining ones are ignored.")
      (Some(specs.head), otherFormulas)
    }
  }

  private[this] final def generateRioConfigFromCLI(): RioConfig = ???


  private[this] final def parseArgs(args: Seq[String]): Any = {
    var args0 = args
    while (args0.nonEmpty) {
      args0 match {
        case Seq("-o", l, rest@_*) =>
          args0 = rest
          outOperatorParameter = Some(l)
        case Seq("-p", p, rest@_*) =>
          args0 = rest
          parameterNames = parameterNames + p
        case Seq(f) =>
          args0 = Seq.empty
          inputFileName = f
        case Seq(f, o) =>
          args0 = Seq.empty
          inputFileName = f
          outputFileName = Some(o)
        case _ => throw new IllegalArgumentException("Unrecognized arguments.")
      }
    }
  }

  private[this] final def printVersion(): Unit = {
    println(s"$name $version")
  }

  private[this] final def usage(): Unit = {
    println(s"usage: $name [-o <operator>] [-p <parameter>] <problem file> [<output file>]")
    println(
      """
        | <problem file> can be either a file name or '-' (without parentheses) for stdin.
        | If <output file> is specified, the result is written to <output file>, otherwise to stdout.
        |
        | Options:
        |  -o <operator>
        |     If <problem file> does not contain a logic specification statement, explicitly set
        |     the output operator to be assumed to <operator>.
        |     This option is ignored, if <problem file> contains a logic specification statement.
        |     Supported <operator>s are: out1, out2, out3, out4
        |
        |  -p <parameter>
        |     Set additional parameters to the output operator.
        |     This option is ignored, if <problem file> contains a logic specification statement.
        |     Supported <parameter>s are: throughput, constrained, credulous, skeptical
        |
        |  --version
        |     Prints the version number of the executable and terminates.
        |
        |  --help
        |     Prints this description and terminates.
        |""".stripMargin)
  }
}
