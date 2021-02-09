package net.aurelee

import leo.datastructures.TPTP
import net.aurelee.rio.core.{ConstrainedCredulous, ConstrainedSetting, ConstrainedSkeptical, OutOperator, RioConfig}

package object rio {
  protected[rio] class UnknownParameterException(val parameterName: String, val allowedParameters: String) extends RuntimeException
  protected[rio] class MalformedLogicSpecificationException(message: String) extends RuntimeException(message)
  protected[rio] class UnspecifiedLogicException extends RuntimeException
  protected[rio] class UnsupportedLogicException(val logic: String) extends RuntimeException

  final def generateRioConfigFromSpec(spec: TPTP.AnnotatedFormula): RioConfig = {
    import leo.datastructures.TPTP.THF
    import reasoner.{Out1, Out2, Out3, Out4}
    spec match {
      case TPTP.THFAnnotated(_, "logic", THF.Logical(f), _) => f match {
        case THF.BinaryFormula(THF.:=, THF.FunctionTerm("$iol", Seq()), THF.Tuple(elements)) if elements.nonEmpty =>
          val parameterMap = tupleElementsToMap(elements)
          if (parameterMap.isDefinedAt("$output")) {
            val throughput = parameterMap.get("$throughput").fold(false) {
              case "$false" => false
              case "$true" => true
              case value => throw new MalformedLogicSpecificationException(s"Only '$$true' or '$$false' allowed as " +
                s"values for parameter '$$throughput', but '$value' was given.")
            }
            val constrained: Option[ConstrainedSetting] = parameterMap.get("$constrained").flatMap {
              case "$credulous" => Some(ConstrainedCredulous)
              case "$skeptical" => Some(ConstrainedSkeptical)
              case value => throw new MalformedLogicSpecificationException(s"Only '$$credulous' or '$$skeptical' allowed as " +
                s"values for parameter '$$constrained', but '$value' was given.")
            }
            val outputoperator: OutOperator = parameterMap("$output") match {
              case "$out1" => Out1
              case "$out2" => Out2
              case "$out3" => Out3
              case "$out4" => Out4
              case name => throw new UnsupportedLogicException(name)
            }
            RioConfig(outputoperator, throughput, constrained)
          } else throw new UnspecifiedLogicException
        case _ => throw new MalformedLogicSpecificationException(s"Cannot read IO logic specification, only entries " +
          s"of form '$$iol := <...>' are legal: ${spec.pretty}")
      }
      case _ => throw new MalformedLogicSpecificationException(s"Cannot read logic specification: ${spec.pretty}")
    }
  }

  private[this] final def tupleElementsToMap(tupleElements: Seq[TPTP.THF.Formula]): Map[String, String] = {
    import TPTP.THF._
    import scala.collection.mutable
    val resultMap: mutable.Map[String, String] = mutable.Map.empty
    tupleElements.foreach {
      case BinaryFormula(:=, FunctionTerm(key, Seq()), FunctionTerm(value, Seq())) =>
        resultMap += (key -> value)
      case entry => throw new MalformedLogicSpecificationException(s"Malformed logic specification entry: ${entry.pretty}")
    }
    resultMap.toMap
  }
}
