package MinecraftCalculator;

import util.control.Breaks._
import scala.collection.mutable.Set
import scala.math
import java.math.MathContext

// mildly cursed

protected sealed trait Expression {
    val value: Any
    def crunch: Expression = this
    override def toString: String = value.toString
    def wrap(s: Any, left: String = "(", right: String = ")"): String =
        s"$left$s$right"
}

protected case class Number(val value: BigDecimal) extends Expression

protected case class Operator(val value: String) extends Expression

protected case class Punct(val value: String) extends Expression

protected case class Operation(val value: (Operator, Expression, Expression)) extends Expression {
    override def crunch = {
        val left = value._2.crunch.asInstanceOf[Number].value
        val right = value._3.crunch.asInstanceOf[Number].value
        new Number(value._1.value match {
            case "^" => left.pow(right.toInt) // if right is 2 big 2 int then whatever
            case "*" => left * right
            case "/" => left / right
            case "+" => left + right
            case "-" => left - right
            case _ => throw new Math.StrangeOperator(value._1)
        })
    }
    override def toString = value._2.value + value._1.value + value._3.value
}

protected case class Paren(val value: Seq[Expression], val dbg: Boolean = false) extends Expression {
    override def crunch: Expression = {
        if (dbg)
            println(this)
        for (window <- value.sliding(2))
            for (clazz <- List("Operator", "Number").map(i => Class.forName("MinecraftCalculator." + i)))
                if (window.forall(clazz.isInstance(_)))
                    throw new Math.TwoInARow(clazz, window(0), window(1))
        if (value.length == 1)
            value(0)
        else
            findParen(value) match {
                case Some((first, last)) => {
                    val innerParen = value.slice(first+1, last)
                    new Paren(value.patch(first, List(new Paren(innerParen, dbg).crunch), innerParen.length+2), dbg).crunch
                }
                case None => findOperator() match {
                    case Some(exp) => exp.crunch
                    case None => this
                }
            }
    }

    def findParen(toks: Seq[Expression]): Option[(Int, Int)] = {
        var first = -1
        var last = -1
        var tally = 0
        breakable {
            for (i <- 0 to toks.length-1 if toks(i).isInstanceOf[Punct])
                toks(i).value match {
                    case "(" => {
                        tally += 1
                        if (tally == 1)
                            first = i
                    }
                    case ")" => {
                        tally -= 1
                        if (tally == 0) {
                            last = i
                            break
                        }
                    }
                }
        }
        if (first == -1 || last == -1)
            return None
        Some((first, last))
    }

    def findOperator(ops: Seq[String] = List("^", "*", "/", "+", "-"), exps: Seq[Expression] = value): Option[Expression] =
        ops match {
            case op :: rest => {
                exps.indexWhere(e => e.isInstanceOf[Operator] && e.value == op) match {
                    case i if i != -1 => {
                        val bef = try exps(i-1) catch {
                            case _: ArrayIndexOutOfBoundsException => throw new Math.ExpectedOperand("before", exps(i))
                        }
                        val aft = try exps(i+1) catch {
                            case _: IndexOutOfBoundsException => throw new Math.ExpectedOperand("after", exps(i))
                        }
                        println(bef)
                        println(aft)
                        val op = new Operation((exps(i).asInstanceOf[Operator], bef, aft))
                        Some(new Paren(exps.patch(i-1, List(op.crunch), 3), dbg))
                    }
                    case -1 => findOperator(rest)
                }
            }
            case List() => None
        }

    override def toString: String = wrap(value.mkString(" "))
}
