package u06lab.solution

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.{Assertions, Test}

class CombinerTests {
  @Test
  def testFunctions() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    println(0.0, f.sum(List())) // 0.0
    println("abc", f.concat(Seq("a", "b", "c")))
    println("", f.concat(Seq()))
    println(3, f.max(List(-10, 3, -5, 0)))
    println(Integer.MIN_VALUE, f.max(List()))
  }

  @Test
  def testFunctionsCombiner() {
    val f: Functions = FunctionsImplCombiner
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    println(0.0, f.sum(List())) // 0.0
    println("abc", f.concat(Seq("a", "b", "c")))
    println("", f.concat(Seq()))
    println(3, f.max(List(-10, 3, -5, 0)))
    println(Integer.MIN_VALUE, f.max(List()))
  }
}