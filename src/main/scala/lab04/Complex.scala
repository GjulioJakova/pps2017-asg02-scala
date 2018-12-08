package lab04

import lab04.Complex.ComplexImplCase

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

object Complex {
  def apply(re:Double, im:Double): Complex =  new ComplexImpl(re, im)

  private class ComplexImpl( var re: Double, var im: Double) extends Complex{

    override def +(c: Complex): Complex =
      Complex(this.re+c.re, this.im+c.im)


    override def *(c: Complex): Complex =
      Complex(this.re*c.re - this.im*c.im, this.re*c.im + this.im*c.re)

    override def toString = "ComplexImpl(" + re + ", " + im + ")"

  }

  case class ComplexImplCase(re: Double, im: Double) extends Complex {
    override def +(c: Complex): Complex = Complex(this.re+c.re, this.im+c.im)
    override def *(c: Complex): Complex = Complex(this.re*c.re - this.im*c.im, this.re*c.im + this.im*c.re)
  }
}

object TryComplex extends App {
  val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val c = a(0)+a(1)+a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0)*a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)

  println(Complex(10,20) == Complex(10,20)) // false
  println(ComplexImplCase(10,20) == ComplexImplCase(10,20)) //true
  println(ComplexImplCase(10,20)) // ComplexImplCase(10.0,20.0)
}

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */