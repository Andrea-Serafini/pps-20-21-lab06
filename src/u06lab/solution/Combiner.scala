package u06lab.solution

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = a.foldLeft(0.0)((x,y)=>x+y)

  override def concat(a: Seq[String]): String = a.foldLeft("")((x,y)=>x.concat(y))

  override def max(a: List[Int]): Int = a.foldLeft(Int.MinValue)((x,y)=>if (x<y) y else x)
}


/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object ImplicitCombiner {
  implicit object sumCombiner extends Combiner[Double] {
    override def unit = 0.0
    override def combine(a: Double, b: Double): Double = a+b
  }
  implicit object concatCombiner extends Combiner[String] {
    override def unit = ""
    override def combine(a: String, b: String): String = a.concat(b)
  }
  implicit object maxCombiner extends Combiner[Int] {
    override def unit = Int.MinValue
    override def combine(a: Int, b: Int): Int = if (a<b) b else a
  }
}

object FunctionsImplCombiner extends Functions {

  import ImplicitCombiner._

  override def sum(a: List[Double]): Double = combine(a)

  override def concat(a: Seq[String]): String = combine(a)

  override def max(a: List[Int]): Int = combine(a)

  def combine[A:Combiner](a: Seq[A]): A = a.foldLeft(implicitly[Combiner[A]].unit)(implicitly[Combiner[A]].combine)
}

object TryFunctions extends App {
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648

  val fComb: Functions = FunctionsImplCombiner
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648
}