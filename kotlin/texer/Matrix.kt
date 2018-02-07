package texer.vector

import texer.rational.*

fun ratArrayOf(vararg rs: Rational): RationalArray {
    var arr = RationalArray(rs.size)
    for(i in 1..(rs.size - 1)) {
        arr[i] = rs[i]
    }
    return arr
}

class RationalArray(val size: Int) {
    
    val numers: IntArray
    val denoms: IntArray
    val iter: RatIterator

    constructor() : this(0) {}

    init {
        numers = IntArray(size)
        denoms = IntArray(size)
        iter = RatIterator(this)
    }
    
    operator fun get(index: Int): Rational {
        return numers[index] R denoms[index]
    }

    operator fun<Rational> iterator(): RatIterator { 
        return iter
    }

}

class MutRationalArray(val size: Int) : RationalArray {

    operator fun set(index: Int, value: Rational) {
        numers[index] = value.numer
        denoms[index] = value.denom
    }

}

class RatIterator(val arr: RationalArray) : Iterator<Rational> {
    
    val numIter: IntIterator
    val denIter: IntIterator

    init {
        numIter = arr.numers.iterator()
        denIter = arr.denoms.iterator()
    }
    override operator fun hasNext(): Boolean {
        return numIter.hasNext()
    }

    override operator fun next(): Rational {
        return numIter.next() R denIter.next()
    }
}

typealias Vector = RationalArray

fun vectorOf(vararg rs: Rational): Vector = ratArrayOf(*rs)

fun vectorSum(v1: Vector, v2: Vector): Vector {
    return ratArrayOf()
}

fun showTexVector(v: Vector, spaces: Boolean): String {
    if(v.length == 0) return "\n\\\\"
    StringBuilder sb = StringBuilder("\n\t")
    for(i in 0..v.length-2) {
        sb.append(v[i].showTex())
        sb.append("&")
    }
    sb.append(v[v.length-1]).showTex()
    sb.append(" \\\\")
    return sb.toString()
}

// note: matrix is immutable
class Matrix(val rows: Array<RationalArray>) {
    
}

class MutMatrix(val rows: Array<RationalArray>) : Matrix {

}
