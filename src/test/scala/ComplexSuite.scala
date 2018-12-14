import lab04.Complex
import org.scalatest.FlatSpec

class ComplexSuite extends FlatSpec{

  val complex = Complex(10,20)

  "Complex number (10,20)" should "have 10 as real and 20 as imaginary part" in {
    assert(complex.re == 10)
    assert(complex.im == 20)
  }

  val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val c = a(0)+a(1)+a(2)

  "Sum between Complex(10,20), Complex(1,1), Complex(7,0)" should "have 18 as real and 21 as imaginary part" in{
    assert(c.re == 18.0)
    assert(c.im == 21.0)
  }
}
