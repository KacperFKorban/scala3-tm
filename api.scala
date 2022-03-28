package tm

sealed trait Move
case class L() extends Move
case class R() extends Move

sealed trait TMs
case class TM[s0 <: Int, actions <: Actions]() extends TMs

sealed trait Actions
case class Nl() extends Actions
case class Cons[a <: (Int, String, Int, String, Move), rest <: Actions]() extends Actions
infix type :+:[a <: (Int, String, Int, String, Move), rest <: Actions] = Cons[a, rest]

type Eval[tm <: TMs, in <: String] = TMImpl.Eval[tm, in]
