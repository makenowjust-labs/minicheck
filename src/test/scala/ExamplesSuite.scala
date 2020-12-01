import minitest.SimpleTestSuite
import minicheck._

object SimpleTestSuite extends SimpleTestSuite {
  test("Example 1: Finding a Counterexample and Shrinking") {
    assertEquals(findCounterExampleNoShrink(intGen)(_ > 0), Some(-1002642896))
    assertEquals(findCounterExample(intGen)(_ > 0), Some(-1))
  }

  test("Example 2: Generaing and Shrinking Higher-Order Functions") {
    val result = findCounterExample(
      funGen(functionCogen(intGen, booleanCogen), booleanGen)
    ) { f => f(_ - 1000000000 >= 0) == f(_ >= 1000000000) }
    assertEquals(result.toString, "Some({case {case -1147483649 => false} => true; case _ => false})")
  }

  test("Example 3: Generating and Shrinking a Function Accepts a Recursive Structure") {
    val result = findCounterExample(funGen(listCogen(booleanCogen), intGen)) { f =>
      f(List(true, false)) == f(List(false, true))
    }
    assertEquals(result.toString, "Some({case List(true, false) => 1; case _ => -1})")
  }
}
