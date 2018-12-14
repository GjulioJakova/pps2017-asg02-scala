package lab05

/** Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly. To apply DRY principle at the best,
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

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object MonadImplicits{

  implicit object IntegerCombiner extends Combiner[Int]{
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = math.max(a, b)
  }

  implicit object StringCombiner extends Combiner[String] {
    override def unit: String = ""
    override def combine(a: String, b: String): String = a.concat(b)
  }

  implicit object DoubleCombiner extends Combiner[Double] {
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b
  }

}

object FunctionsImpl extends Functions {

    import MonadImplicits._

    private def combine[A](collection: List[A], combiner: Combiner[A]): A = {
      var result: A = combiner.unit
      collection.foreach( elem => {
        result = combiner.combine(result,elem)
      })
        result
    }

    override def sum(a: List[Double]): Double = combine(a, DoubleCombiner)

    override def concat(a: Seq[String]): String = combine(a.toList, StringCombiner)

    override def max(a: List[Int]): Int = combine(a, IntegerCombiner)

  }

object TryFunctions extends App {
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648
}