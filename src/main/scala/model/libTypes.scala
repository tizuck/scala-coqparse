package model

object libTypes {
  object acyclicGraph {
    sealed trait ConstraintType
    case object Lt extends ConstraintType
    case object Le extends ConstraintType
    case object Eq extends ConstraintType
  }

  object util {
    type Union [A,B] = cLibTypes.cSig.Union[A,B]
  }

  object flags {
    sealed trait CompatVersion
    case object V8_8 extends CompatVersion
    case object V8_9 extends CompatVersion
    case object Current extends CompatVersion
  }

}
