package net.aurelee

import leo.datastructures.TPTP
import net.aurelee.rio.core.{Formula, OutOperator, interpretTFFTermAsFormula}
import net.aurelee.rio.reasoner.{CredulousNetOutput, NetOutputFunction, SkepticalNetOutput, RioConfig}

package object rio {
  protected[rio] class MalformedLogicSpecificationException(message: String) extends RuntimeException(message)
  protected[rio] class SemanticsException(message: String) extends RuntimeException(message)
  protected[rio] class UnspecifiedLogicException extends RuntimeException
  protected[rio] class UnsupportedLogicException(logic: String) extends RuntimeException(logic)

  final def generateRioConfigFromSpec(spec: TPTP.AnnotatedFormula): RioConfig = {
    import leo.datastructures.TPTP.TFF
    import reasoner.{Out1, Out2, Out3, Out4}
    spec match {
      case TPTP.TFFAnnotated(_, "logic", TFF.Logical(f), _) => f match {
        case TFF.MetaIdentity(TFF.AtomicTerm("$$iol", Seq()), TFF.Tuple(elements)) if elements.nonEmpty =>
          val parameterMap = tupleElementsToMap(elements)
          if (parameterMap.isDefinedAt("$$operator")) {
            val throughput = parameterMap.get("$$throughput").fold(false) {
              case TFF.AtomicTerm("$false", Seq()) => false
              case TFF.AtomicTerm("$true", Seq()) => true
              case value => throw new MalformedLogicSpecificationException(s"Only '$$true' or '$$false' allowed as " +
                s"values for parameter '$$$$throughput', but '${value.pretty}' was given.")
            }
            val constrained: Option[NetOutputFunction] = parameterMap.get("$$constrained").flatMap {
              case TFF.AtomicTerm("$$credulous", Seq()) => Some(CredulousNetOutput)
              case TFF.AtomicTerm("$$skeptical", Seq()) => Some(SkepticalNetOutput)
              case value => throw new MalformedLogicSpecificationException(s"Only '$$$$credulous' or '$$$$skeptical' allowed as " +
                s"values for parameter '$$$$constrained', but '${value.pretty}' was given.")
            }
            val constraints: Seq[Formula] = parameterMap.get("$$constraints").fold(Seq.empty[Formula]){
              case TFF.Tuple(elements) if elements.nonEmpty => elements.map(interpretTFFTermAsFormula)
              case value => throw new MalformedLogicSpecificationException(s"Only tuples of formulas are allowed as " +
                s"values for parameter '$$$$constraints', but '${value.pretty}' was given.")
            }
            val outputoperator: OutOperator = parameterMap("$$operator") match {
              case TFF.AtomicTerm("$$out1", Seq()) => Out1
              case TFF.AtomicTerm("$$out2", Seq()) => Out2
              case TFF.AtomicTerm("$$out3", Seq()) => Out3
              case TFF.AtomicTerm("$$out4", Seq()) => Out4
              case name => throw new UnsupportedLogicException(name.pretty)
            }
            RioConfig(outputoperator, throughput, constrained, constraints)
          } else throw new UnspecifiedLogicException
        case _ => throw new MalformedLogicSpecificationException(s"Cannot read IO logic specification, only entries " +
          s"of form '$$iol == ...' are legal: ${spec.pretty}")
      }
      case _ => throw new MalformedLogicSpecificationException(s"Only TFF formulas of role 'logic' are supported in logic " +
        s"specification, but '${spec.pretty}' was given.'")
    }
  }

  private[this] final def tupleElementsToMap(tupleElements: Seq[TPTP.TFF.Term]): Map[String, TPTP.TFF.Term] = {
    import TPTP.TFF
    import scala.collection.mutable
    val resultMap: mutable.Map[String, TPTP.TFF.Term] = mutable.Map.empty
    tupleElements.foreach {
      case TFF.FormulaTerm(TFF.MetaIdentity(TFF.AtomicTerm(key, Seq()), right)) =>
        resultMap += (key -> right)
      case entry => throw new MalformedLogicSpecificationException(s"Malformed logic specification entry: ${entry.pretty}")
    }
    resultMap.toMap
  }
}
