package model

import model.kernelTypes.{globRef, id}

object engineTypes {

  object evarKinds {

    sealed trait T

    sealed case class ImplicitArg(t: globRef.T, tu: (Int, id.T)) extends T

    sealed case class BinderType(n:kernelTypes.name.T) extends T

  }


  object namegen {

    sealed trait IntroPatternNamingExpr

    sealed case class IntroIdentifier(iT: id.T) extends IntroPatternNamingExpr

    sealed case class IntroFresh(iT: id.T) extends IntroPatternNamingExpr

    case object IntroAnonymous extends IntroPatternNamingExpr

  }

  object uState {
    sealed case class GenUniverseDecl [A,B] (univdeclInstance:A,
                                             univdeclExtensibleInstance:Boolean,
                                             univdeclConstraints:B,
                                             UnivdeclExtensibleConstraint:Boolean)

  }

}
