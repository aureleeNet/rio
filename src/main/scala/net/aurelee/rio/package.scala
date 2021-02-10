package net.aurelee

import leo.datastructures.TPTP
import net.aurelee.rio.core.{Formula, OutOperator, interpretFormula}
import net.aurelee.rio.reasoner.{CredulousNetOutput, NetOutputFunction, SkepticalNetOutput, RioConfig}

package object rio {
  protected[rio] class UnknownParameterException(val parameterName: String, val allowedParameters: String) extends RuntimeException
  protected[rio] class MalformedLogicSpecificationException(message: String) extends RuntimeException(message)
  protected[rio] class SemanticsException(message: String) extends RuntimeException(message)
  protected[rio] class UnspecifiedLogicException extends RuntimeException
  protected[rio] class UnsupportedLogicException(logic: String) extends RuntimeException(logic)

  final def generateRioConfigFromSpec(spec: TPTP.AnnotatedFormula): RioConfig = {
    import leo.datastructures.TPTP.THF
    import reasoner.{Out1, Out2, Out3, Out4}
    spec match {
      case TPTP.THFAnnotated(_, "logic", THF.Logical(f), _) => f match {
        case THF.BinaryFormula(THF.:=, THF.FunctionTerm("$iol", Seq()), THF.Tuple(elements)) if elements.nonEmpty =>
          val parameterMap = tupleElementsToMap(elements)
          if (parameterMap.isDefinedAt("$output")) {
            val throughput = parameterMap.get("$throughput").fold(false) {
              case THF.FunctionTerm("$false", Seq()) => false
              case THF.FunctionTerm("$true", Seq()) => true
              case value => throw new MalformedLogicSpecificationException(s"Only '$$true' or '$$false' allowed as " +
                s"values for parameter '$$throughput', but '${value.pretty}' was given.")
            }
            val constrained: Option[NetOutputFunction] = parameterMap.get("$constrained").flatMap {
              case THF.FunctionTerm("$credulous", Seq()) => Some(CredulousNetOutput)
              case THF.FunctionTerm("$skeptical", Seq()) => Some(SkepticalNetOutput)
              case value => throw new MalformedLogicSpecificationException(s"Only '$$credulous' or '$$skeptical' allowed as " +
                s"values for parameter '$$constrained', but '${value.pretty}' was given.")
            }
            val constraints: Seq[Formula] = parameterMap.get("$constraints").fold(Seq.empty[Formula]){
              case THF.Tuple(elements) if elements.nonEmpty => elements.map(interpretFormula)
              case value => throw new MalformedLogicSpecificationException(s"Only tuples of formulas are allowed as " +
                s"values for parameter '$$constraints', but '${value.pretty}' was given.")
            }
            val outputoperator: OutOperator = parameterMap("$output") match {
              case THF.FunctionTerm("$out1", Seq()) => Out1
              case THF.FunctionTerm("$out2", Seq()) => Out2
              case THF.FunctionTerm("$out3", Seq()) => Out3
              case THF.FunctionTerm("$out4", Seq()) => Out4
              case name => throw new UnsupportedLogicException(name.pretty)
            }
            RioConfig(outputoperator, throughput, constrained, constraints)
          } else throw new UnspecifiedLogicException
        case _ => throw new MalformedLogicSpecificationException(s"Cannot read IO logic specification, only entries " +
          s"of form '$$iol := <...>' are legal: ${spec.pretty}")
      }
      case _ => throw new MalformedLogicSpecificationException(s"Cannot read logic specification: ${spec.pretty}")
    }
  }

  private[this] final def tupleElementsToMap(tupleElements: Seq[TPTP.THF.Formula]): Map[String, TPTP.THF.Formula] = {
    import TPTP.THF
    import scala.collection.mutable
    val resultMap: mutable.Map[String, TPTP.THF.Formula] = mutable.Map.empty
    tupleElements.foreach {
      case THF.BinaryFormula(THF.:=, THF.FunctionTerm(key, Seq()), right) =>
        resultMap += (key -> right)
      case entry => throw new MalformedLogicSpecificationException(s"Malformed logic specification entry: ${entry.pretty}")
    }
    resultMap.toMap
  }
}
