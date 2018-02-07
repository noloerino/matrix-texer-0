package utils

import kotlin.collections.ArrayList

fun <T, R, V>zipWith(f: (T, R) -> V, a1: Array<out T>, a2: Array<out R>): ArrayList<V> {
    val len: Int = Math.min(a1.size, a2.size)
    var list: ArrayList<V> = ArrayList(len)
    for(i in 0..len-1) {
        list.add(f(a1[i], a2[i]))
    }
    return list
}
