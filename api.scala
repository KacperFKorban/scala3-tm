package tm

sealed trait Move
case class L() extends Move
case class R() extends Move

sealed trait TMs
case class TM[s0 <: Int, actions <: Actions]() extends TMs

sealed trait Actions
case class Nl() extends Actions
case class Cons[a <: Act, rest <: Actions]() extends Actions
infix type :+:[a <: Act, rest <: Actions] = Cons[a, rest]

sealed trait Act
case class Action[stateFrom <: Int, charFrom <: String, stateTo <: Int, charTo <: String, move <: Move]() extends Act

type Eval[tm <: TMs, in <: String] = TMImpl.Eval[tm, in]
