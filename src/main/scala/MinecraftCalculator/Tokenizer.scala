package MinecraftCalculator;

import util.control.Breaks._
import collection.mutable.ArrayBuffer
import java.text.ParseException

private case class Token(var s: String = "", val t: Tokenizer.Type = null) {
    override def toString: String = s"$t:$s"
    def express =
        t match {
            case Tokenizer.Number => new Number(BigDecimal(s))
            case Tokenizer.Operator => new Operator(s)
            case Tokenizer.Punct => new Punct(s)
        }
}

private object Tokenizer {
    sealed trait Type
    case object Number extends Type
    case object Operator extends Type
    case object Punct extends Type

    val recognize = Map[Type, Char => Boolean](
        Number -> (c => c.isDigit),
        Operator -> (c => "^*/+-".indexOf(c) != -1),
        Punct -> (c => c == '(' || c == ')')
    )

    def apply(math: String): Seq[Token] = {
        checkParens(math)
        var toks = new ArrayBuffer[Token]
        var tok = new Token

        def resetToken(t: Type = null) = {
            if (tok.t != null)
                toks.append(tok)
            tok = new Token(t=t)
        }

        for (c <- math) {
            if (c.isWhitespace)
                resetToken()
            else {
                breakable {
                    for ((t, f) <- recognize)
                        if ((tok.t != t || t == Punct) && f(c)) {
                            resetToken(t)
                            break
                        }
                }
                tok.s += c
            }
        }
        resetToken()

        toks
    }

    def checkParens(math: String) {
        def doit: Option[Int] = {
            var tally = 0
            for (c <- 0 to math.length-1 if math(c) == '(' || math(c) == ')') {
                math(c) match {
                    case '(' => tally += 1
                    case ')' => {
                        tally -= 1
                        if (tally < 0)
                            return Some(c)
                    }
                }
            }
            if (tally == 0)
                None
            else
                Some(math.length-1)
        }
        doit match {
            case Some(ret) =>
                throw new ParseException(s"Couldn't parse «$math» near character #$ret.", ret)
            case None => ()
        }
    }
}
