import scala.language.implicitConversions
import scala.math._


case class CalComplex(re: Double, im: Double) extends Ordered[ CalComplex] {


  def this(re: Double) = this(re, 0)
  def unary_+ : CalComplex = this
  def unary_- = new CalComplex(-re, -im)
  def unary_~ = new CalComplex(re, -im)
  def unary_! : Double = sqrt(pow(re, 2) + pow (im, 2))

  def compare(that: CalComplex): Int = !this compare !that

  def +(c: CalComplex) = new CalComplex(re + c.re, im + c.im)
  def -(c: CalComplex): CalComplex = this + -c
  def *(c: CalComplex) = new CalComplex(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: CalComplex): CalComplex = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new CalComplex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  override def toString: String =
    this match {
      case  CalComplex.i => "i"
      case CalComplex(re, 0) => re.toString
      case CalComplex(0, im) => im.toString + "*i"
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object CalComplex {
  val i = new  CalComplex(0, 1)

  def apply(re: Double) = new  CalComplex(re)


  implicit def fromDouble(d: Double): CalComplex = new  CalComplex(d)
  implicit def fromFloat(f: Float): CalComplex = new  CalComplex(f)
  implicit def fromLong(l: Long): CalComplex = new  CalComplex(l)
  implicit def fromInt(i: Int): CalComplex = new  CalComplex(i)
  implicit def fromShort(s: Short): CalComplex = new  CalComplex(s)
}






