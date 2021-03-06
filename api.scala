package tm

enum Move:
  case L()
  case R()
export Move.*

sealed trait TMs
class TM[s0 <: Int, actions <: Actions] extends TMs

sealed trait Actions
class Nl extends Actions
class :+:[a <: Act, rest <: Actions] extends Actions

sealed trait Act
class Action[stateFrom <: Int, charFrom <: String, stateTo <: Int, charTo <: String, move <: Move] extends Act

type Eval[tm <: TMs, in <: String] = TMImpl.Eval[tm, in]
