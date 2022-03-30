package tm

import scala.compiletime.ops.*
import scala.compiletime.ops.string.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.util.chaining

private[tm] object TMImpl {

  sealed trait Tap
  class Tape[Left <: CharChain, Right <: CharChain] extends Tap

  sealed trait CharChain
  class Eps extends CharChain
  class CharCons[c <: String, rest <: CharChain] extends CharChain
  infix type :|:[c <: String, rest <: CharChain] = CharCons[c, rest]

  type Result[tp <: Tap] = tp match
    case Tape[lt, rt] => ReverseStr[FromCharChain[lt]] + FromCharChain[ReverseChain[RemoveBlanksFront[ReverseChain[rt]]]]

  type RemoveBlanksFront[chain <: CharChain] = chain match
    case Eps => Eps
    case "_" :|: rest => RemoveBlanksFront[rest]
    case _ => chain

  type ReverseStr[str <: String] = str match
    case "" => ""
    case _ => ReverseStr[TailStr[str]] + HeadStr[str]

  type TailStr[str <: String] = Substring[str, 1, Length[str]]

  type HeadStr[str <: String] = Substring[str, 0, 1]

  type HeadChain[chain <: CharChain] = chain match
    case Eps => "_"
    case c :|: _ => c

  type TailChain[chain <: CharChain] = chain match
    case Eps => Eps
    case _ :|: rest => rest

  type CurrentChar[tape <: Tap] = tape match
    case Tape[_, rt] => HeadChain[rt]

  type ToCharChain[str <: String] = str match
    case "" => Eps
    case _ => HeadStr[str] :|: ToCharChain[TailStr[str]]

  type FromCharChain[chain <: CharChain] = chain match
    case Eps => ""
    case c :|: rest => ToString[c] + FromCharChain[rest]

  type ReverseChain[chain <: CharChain] = ReverseChainGo[chain, Eps]

  type ReverseChainGo[chain <: CharChain, acc <: CharChain] = chain match
    case Eps => acc
    case c :|: rest => ReverseChainGo[rest, c :|: acc]

  type Eval[tm <: TMs, in <: String] = tm match
    case TM[s0, actions] => Result[EvalGo[s0, Tape[Eps, ToCharChain[in]], actions]]

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
        case Eps => Tape[Eps, c :|: TailChain[rt]]
        case ltHead :|: ltRest => Tape[ltRest, ltHead :|: c :|: TailChain[rt]]
      case R => Tape[c :|: lt, TailChain[rt]]

}