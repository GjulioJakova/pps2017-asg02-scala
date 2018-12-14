import lab04.Try.{cPCD, cPPS, s1}
import lab04.{Course, Student}
import org.scalatest.FunSuite

class StudentSuite extends FunSuite{

  val s1 = Student("mario",2015)

  test("The student initially has no courses"){
    assert(s1.courses.toSeq.isEmpty)
  }

  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")


  test("Now the student is enrolled in two courses"){
    s1.enrollings(cPPS, cPCD)
    assert(s1.courses.toSeq.size == 2)
  }

  test("One of Mario's teacher is Ricci"){
    assert(s1.hasTeacher("Ricci"))
  }

}
