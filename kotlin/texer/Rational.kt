package texer.rational

import texer.showtex.*

infix fun Int.R(o: Int): Rational = Rational(this, o)

tailrec fun gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a % b)

open class Rational(n: Int, d: Int): Number(), ShowTex {

    val numer: Int
    val denom: Int

    constructor(r: Rational) : this(r.numer, r.denom)

    init {
        val thisGcd: Int = gcd(n, d)
        numer = n / thisGcd
        denom = d / thisGcd
    }

    operator fun unaryMinus() = Rational(-numer, denom)
    
    operator fun inc() = Rational(numer + denom, denom)

    operator fun dec() = Rational(numer - denom, denom)

    operator fun plus(n: Rational) = Rational(numer + n.numer, denom + n.denom)

    operator fun minus(n: Rational) = Rational(numer - n.numer, denom - n.denom)

    operator fun times(n: Rational) = Rational(numer * n.numer, denom * n.denom)

    operator fun div(n: Rational) = Rational(numer * n.denom, denom * n.numer)

    // operator fun rem(n: Rational)

    override fun toString() = "$numer/$denom"

    override fun showTex(): String {
        if(denom == 1) {
            return numer.toString()
        }
        else {
            return fracTag(numer, denom)
        }
    }

    // Required by Number type
    override fun toByte() = toInt().toByte()

    override fun toChar() = toInt().toChar()

    override fun toDouble() = numer.toDouble() / denom.toDouble()

    override fun toFloat() = numer.toFloat() / denom.toFloat()

    override fun toInt() = numer / denom

    override fun toLong() = toInt().toLong()

    override fun toShort() = toInt().toShort()

}

