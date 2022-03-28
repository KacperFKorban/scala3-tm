<h1 align="center">
  Scala 3 Match Type Turing Machine
</h1>
<p align="center">
  Turing Machine using Scala 3 match types
</p>

### Example

We encode a natural number `n` as `n` times '0' followed by a '1'.

Let's declare a TM that increments a natural number.

```scala
import tm.*

type incTM = TM[
  0,
  Action[0, "0", 0, "0", R] :+:
    Action[0, "1", 1, "0", R] :+:
      Action[1, "_", 2, "1", R] :+: Nl
]
```

Then, we can run this TM by
```scala
import scala.compiletime.*

constValue[Eval[incTM, "01"]]
```
