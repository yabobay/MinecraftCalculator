package MinecraftCalculator;

object Math {
    def apply(math: String): String =
        Math(Tokenizer(math), false).toString

    def apply(math: String, dbg: Boolean): String = {
        val toks = Tokenizer(math)
        if (dbg)
            println(toks.toList)
        Math(toks, dbg).toString
    }

    def apply(toks: Seq[Token], dbg: Boolean): BigDecimal =
        new Paren(toks.map(_.express), dbg).crunch.asInstanceOf[Number].value
}
