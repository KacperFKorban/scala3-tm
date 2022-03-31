package tm

import scala.compiletime.ops.*
import scala.compiletime.ops.string.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*

private[tm] object TMImpl {

  sealed trait Tap
  class Tape[Left <: String, Right <: String] extends Tap

  type Result[tp <: Tap] = tp match
    case Tape[lt, rt] => ReverseStr[lt] + ReverseStr[RemoveBlanksFront[ReverseStr[rt]]]

  type RemoveBlanksFront[str <: String] = str match
    case "" => ""
    case _ => HeadStr[str] match
      case "_" => RemoveBlanksFront[TailStr[str]]
      case _ => str

  type ReverseStr[str <: String] = str match
    case "" => ""
    case _ => ReverseStr[TailStr[str]] + HeadStr[str]

  type HeadStrSafe[str <: String] = str match
    case "" => "_"
    case _ => HeadStr[str]

  type TailStrSafe[str <: String] = str match
    case "" => ""
    case _ => TailStr[str]

  type TailStr[str <: String] = Substring[str, 1, Length[str]]

  type HeadStr[str <: String] = Substring[str, 0, 1]

  type CurrentChar[tape <: Tap] = tape match
    case Tape[_, rt] => HeadStrSafe[rt]

  type Eval[tm <: TMs, in <: String] = tm match
    case TM[s0, actions] => Result[EvalGo[s0, Tape["", in], actions]]

  type EvalGo[s <: Int, tape <: Tap, actions <: Actions] = FindAction[s, CurrentChar[tape], actions] match
    case Some[(st, ct, m)] => EvalGo[st, MakeMove[tape, m, ct], actions]
    case None.type => tape

  type FindAction[s <: Int, c <: String, actions <: Actions] = actions match
    case Action[sf, cf, st, ct, m] :+: rest =>
      (sf == s && cf == c) match
        case true => Some[(st, ct, m)]
        case false => FindAction[s, c, rest]
    case Nl => None.type

  type MakeMove[tape <: Tap, m <: Move, c <: String] = tape match
    case Tape[lt, rt] => m match
      case L => lt match
        case "" => Tape["", c + TailStrSafe[rt]]
        case _ => Tape[TailStr[lt], HeadStr[lt] + c + TailStrSafe[rt]]
      case R => Tape[c + lt, TailStrSafe[rt]]

}