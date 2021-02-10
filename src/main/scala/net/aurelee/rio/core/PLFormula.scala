package net.aurelee.rio.core

sealed abstract class PLFormula extends Pretty {
  def conjs: Seq[PLFormula]
  def disjs: Seq[PLFormula]
  def symbols: Set[String]
  def replace(what: PLFormula, by: PLFormula): PLFormula
}
final case class PLNeg(body: PLFormula) extends PLFormula {
  override def pretty: String = body match {
    case PLDisj(_, _) | PLConj(_,_) => s"~ (${body.pretty})"
    case _ => s"~ ${body.pretty}"
  }

  override def conjs: Seq[PLFormula] = Vector(this)
  override def disjs: Seq[PLFormula] = Vector(this)
  override def symbols: Set[String] = body.symbols
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else PLNeg(body.replace(what, by))
}
final case class PLDisj(left: PLFormula, right: PLFormula) extends PLFormula {
  override def pretty: String = {
    left match {
      case PLTop | PLBottom | PLProp(_) | PLNeg(_) => right match {
        case PLTop | PLBottom | PLProp(_) | PLNeg(_) => s"${left.pretty} | ${right.pretty}"
        case _ => s"${left.pretty} | (${right.pretty})"
      }
      case _ => right match {
        case PLTop | PLBottom | PLProp(_) | PLNeg(_) => s"(${left.pretty}) | ${right.pretty}"
        case _ => s"(${left.pretty}) | (${right.pretty})"
      }
    }
  }

  override def conjs: Seq[PLFormula] = Vector(this)
  override def disjs: Seq[PLFormula] = Vector.concat(left.disjs, right.disjs)
  override def symbols: Set[String] = left.symbols union right.symbols
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else PLDisj(left.replace(what, by),right.replace(what, by))
}
final case class PLConj(left: PLFormula, right: PLFormula) extends PLFormula {
  override def pretty: String = {
    left match {
      case PLTop | PLBottom | PLProp(_) | PLNeg(_) => right match {
        case PLTop | PLBottom | PLProp(_) | PLNeg(_) => s"${left.pretty} & ${right.pretty}"
        case _ => s"${left.pretty} & (${right.pretty})"
      }
      case _ => right match {
        case PLTop | PLBottom | PLProp(_) | PLNeg(_) => s"(${left.pretty}) & ${right.pretty}"
        case _ => s"(${left.pretty}) & (${right.pretty})"
      }
    }
  }

  override def conjs: Seq[PLFormula] = Vector.concat(left.conjs, right.conjs)
  override def disjs: Seq[PLFormula] = Vector(this)
  override def symbols: Set[String] = left.symbols union right.symbols
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else PLDisj(left.replace(what, by),right.replace(what, by))
}
final case class PLProp(name: String) extends PLFormula {
  override def pretty: String = name

  override def conjs: Seq[PLFormula] = Vector(this)
  override def disjs: Seq[PLFormula] = Vector(this)
  override def symbols: Set[String] = Set(name)
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else this
}
case object PLTop extends PLFormula {
  override def pretty: String = "$true"

  override def conjs: Seq[PLFormula] = Vector(this)
  override def disjs: Seq[PLFormula] = Vector(this)
  override def symbols: Set[String] = Set.empty
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else this
}
case object PLBottom extends PLFormula {
  override def pretty: String = "$false"

  override def conjs: Seq[PLFormula] = Vector(this)
  override def disjs: Seq[PLFormula] = Vector(this)
  override def symbols: Set[String] = Set.empty
  override def replace(what: PLFormula, by: PLFormula): PLFormula = if (this == what) by else this
}
