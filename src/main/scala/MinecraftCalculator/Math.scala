package MinecraftCalculator;

object Math {
    @throws[SyntaxError]
    def apply(math: String): String =
        Math(Tokenizer(math), false).toString

    @throws[SyntaxError]
    def apply(math: String, dbg: Boolean): String = {
        val toks = Tokenizer(math)
        if (dbg)
            println(toks.toList)
        Math(toks, dbg).toString
    }

    def apply(toks: Seq[Token], dbg: Boolean): BigDecimal =
        new Paren(toks.map(_.express), dbg).crunch.asInstanceOf[Number].value

    class SyntaxError extends Throwable {
        override def toString = "I don't know‽ Weird error!"
    }

    class ExpectedOperand(val where: String, val operator: Any) extends SyntaxError {
        override def toString = s"Expected operand $where ‘$operator’"
    }

    class StrangeOperator(val op: Operator) extends SyntaxError {
        override def toString = s"Unknown operator ‘$op’"
    }

    class TwoInARow(val clazz: Class[_], val op1: Expression, val op2: Expression) extends SyntaxError {
        override def toString = s"Two ${clazz.getSimpleName.toLowerCase}s in a row: $op1 $op2"
    }
}
