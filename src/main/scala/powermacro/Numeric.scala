package powermacro

trait Numeric[A] {
    val one: A
    def mult(x: A, y: A): A
}

object Numeric {
    def apply[A : Numeric]: Numeric[A] = summon

    implicit val int: Numeric[Int] = new Numeric[Int] {
        override val one = 1
        override def mult(x: Int, y: Int): Int = x * y
    }
    implicit val double: Numeric[Double] = new Numeric[Double] {
        override def mult(x: Double, y: Double): Double = x * y
        override val one = 1.0D
    }

    extension [A : Numeric](x: A) {
        def *(y: A): A = Numeric[A].mult(x, y)
    }
}

inline def pow[Num](x: Num, inline n: Int)(using num: Numeric[Num]): Num = 
    ${ PowerMacro.powerMacro('x, 'n)(using 'num) }