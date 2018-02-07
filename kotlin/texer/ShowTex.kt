package texer.showtex

interface ShowTex {
    public fun showTex(): String
}

fun tag(tag: String, vararg xs: Any?): String {
    var sb: StringBuilder = StringBuilder("\\" + tag)
    for(s in xs) {
        sb.append("{", s, "}")
    }
    return sb.toString()
}

fun fracTag(n1: Any?, n2: Any?) = tag("frac", n1, n2)

fun sqrtTag(n: Any?) = tag("sqrt", n)
