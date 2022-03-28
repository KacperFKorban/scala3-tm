//> using lib "org.scalameta::munit:0.7.29"

import tm.*

import scala.compiletime.*

class TMSuite extends munit.FunSuite {
  test("simple tm") {
    type tm = TM[0, Action[0, "0", 0, "0", R] :+: Nl]
    assertEquals(constValue[Eval[tm, "0"]], "0")
  }

  test("increment") {
    type incTM = TM[0, Action[0, "0", 0, "0", R] :+: Action[0, "1", 1, "0", R] :+: Action[1, "_", 2, "1", R] :+: Nl]
    assertEquals(
      constValue[Eval[incTM, "01"]],
      "001"
    ) 
  }
}
