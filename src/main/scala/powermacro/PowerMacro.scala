package powermacro

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import Numeric._

object PowerMacro {
    def powerMacro[Num : Type](x: Expr[Num], n: Expr[Int])(using num: Expr[Numeric[Num]])(using q: Quotes): Expr[Num] = {
        val pow = n.valueOrAbort

        val res = if(pow == 0) '{ $num.one }
        else if(pow % 2 == 0) '{
            given Numeric[Num] = $num
            val xPow = ${powerMacro(x, Expr(pow / 2))}
            xPow * xPow
        } else '{ 
            given Numeric[Num] = $num
            $x * ${powerMacro(x, Expr(pow - 1))}
        }

        println(res.show)
        res
    }
}
