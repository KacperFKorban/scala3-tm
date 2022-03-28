//> using scala "3.nightly"

import tm.*

import scala.compiletime.*

@main
def main =
  type tm = TM[0, Action[0, "0", 0, "0", R] :+: Nl]

  println(
    constValue[Eval[tm, "0"]]
  )
