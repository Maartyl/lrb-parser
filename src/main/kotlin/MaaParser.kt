package cz.maa.parser

import org.intellij.lang.annotations.Language
import java.io.Reader
import java.io.StringReader
import java.io.StringWriter
import java.io.Writer
import java.lang.IllegalStateException
import kotlin.reflect.KProperty

// my own parser, that handles eft recursion fine
/*

 name: Naive LeftReducing Backtracking Parser / ParserGenerator(future)
 :: dubbed LRB

* */

//imagined usage

//term("regex") : Parser<string>
// - BETTTER: Parser<RegexMatch>
//term("regex", str=>T) : Parser<T>

//ALSO!!! somehow pass line nubers, etc.
// - maybe that will just e on some kind of ParserContext

//nonterminal... or compose
//nt("name").alt(parser..., (...)=>T) : Parser<T>
// - will need all the overloads for special generics, fine

//parser itself can: recursively fix itself to remove left recursion
// - rules with only left recursion will throw error
//

//2 VERSION::
// PushParse(c :char, x: Context):NextParser / failed
// and
// PullParse(s: CharSource): T or error
// -- this one could be recursive descent..
// I want to TRY make the FIRST
// -- that pushes state and such, has explicit stack...
// -- instead of returning, will just sometimes

// SOOO.... yeah, cannot do the pushing one...
// - oh well; still should be nice

//IMPORTANTLY: must keep the buffer of unparsed stuff...

//class T
//
//fun T.alt(t: T, f: (T) -> T): T = t
//
//val t = T()
//
//val x = t.alt(t) {
//    it
//}.alt(t) {
//    it
//}

// ... hmm... stateful recursive rules? dammit...
// - needs some kind of stack...

object TEST : Factory() {


    @JvmStatic
    fun main(args: Array<String>) {

        StringReader("1a5aa6 28b3").use {
            //val yy = Context(it)
            val p = mgram()
            println("\n---------")
            val rslt = p.parseAll(it).take(10).toList()
            println(rslt)
        }
    }

    private fun mgram(): LRBParser<Node> {
        val num = term("[0-9]+")
        val ows = term("[ \n\t]*")

        val expr = rule<Node>("expr")

        expr
            .alt(expr, term("[abc]+"), expr) { _, l, o, r ->
                //Node.Y(Node.Leaf("."), o, r)
                Node.Y(l, o, r)
            }
            .alt(num) { _, n -> Node.Leaf(n) }

        val root = rule<Node>("root")
            .alt(expr, ows) { _, l, _ -> l }

        return fin(root)
    }

    private fun simplest(): LRBParser<Node> {
        return fin(term("[0-9]+") { _, m -> Node.Leaf(m.value) })
    }

    private fun alt2(): LRBParser<Node> {
        val e = rule<Node>("e")
        val num: Parser<Node> = term("[0-9]+") { _, m -> Node.Leaf("I:" + m.value) }
        val alf: Parser<Node> = term("[a-z]+") { _, m -> Node.Leaf("A:" + m.value) }

        e.alt(num)
        e.alt(alf)
        return fin(e)
    }

//    private fun fin(expr: Parser<Node>) = rule<Node>("root")
//        .alt(expr).compileSubtree(emptySet())

    private fun fin(expr: Parser<Node>) = LRBParser(rule<Node>("root").alt(expr))

    sealed class Node {
        data class Leaf(val s: String) : Node() {
            override fun toString() = "($s)"
        }

        data class Y(
            val l: Node,
            val op: String,
            val r: Node
        ) : Node() {
            override fun toString() = "($l $op $r)"
        }
    }

}

class LRBParser<T : Any>(rootParserDefinition: Parser<T>) {
    private val p = compileParser(rootParserDefinition)

    @Suppress("UNCHECKED_CAST")
    fun parseOne(r: Reader) = Context(r).let { yy ->
        p.parse(frameEmpty, yy, Stack.top)?.also {
            yy.informParsedTopLevel()
        } as? T
    }

    @Suppress("ReplaceSingleLineLet", "UNCHECKED_CAST")
    fun parseAll(r: Reader) = Context(r).let { yy ->
        generateSequence {
            p.parse(frameEmpty, yy, Stack.top)?.let {
                yy.informParsedTopLevel()
                it as T
            }
        }
    }
}

open class Factory {
    class R<T : Any> {
        private val self = mutableMapOf<String, Alt<T>>()
        operator fun getValue(thisRef: Any?, property: KProperty<*>): Alt<T> {
            return self[property.name] ?: rule<T>(property.name).also {
                self[property.name] = it
            }
        }
    }

    companion object {
        /**
        IMPORTANT: must not use MatchResult after leaving wrap
        -- MatchResult becomes unusable, and you may not retain reference to it
         */
        fun <T : Any> term(@Language("RegExp") rgx: String, name: String? = null, wrap: (Pin, MatchResult) -> T) =
            Terminal(name, Regex("\\A$rgx"), wrap)

        fun term(@Language("RegExp") rgx: String, name: String? = null) = term(rgx, name) { _, m -> m.value }
        fun <T : Any> rule(name: String) = Alt<T>(name)

        fun <T : Any> lrb(root: Parser<T>) = LRBParser(root)

        //DANGEROUS:: when in loop: will never move -> infinite loop
        fun <T : Any> Parser<T>.opt(dflt: T) =
            rule<T>("anon_${this.name}_opt")
                .alt(this)
                .alt(Chain("empty", emptyList()) { _, _ -> dflt })
    }

}


//TODO: will include: col,row, sbOffset after allowed to delete too old.. etc.
data class Pin(
    val pos: Int,
    val row: Int,
    val col: Int,
    //who it came from + provides context to handlers
    // - Pin will implement some intrerface exposed to handlers
    private val yy: Context
)

class Stack(
    val next: Stack?,
    val creator: Named,
    val pin: Pin? = null,
    var chainPos: Int? = null
) {

    fun showInto(w: Writer) {
        next?.showInto(w)
        w.write(" / ")
        pin?.let { w.write("(${it.pos})") }
        w.write(creator.name)
        chainPos?.let { w.write(" .$it") }
    }

    override fun toString() =
        StringWriter().use { showInto(it); it.toString() }

    companion object {
        val top = Stack(null, object : Named {
            override val name = "-"
        })
    }
}


//mutable; not thread safe
class Context(private val src: Reader) : CharSequence {
    private val cbuf = CharArray(2048) //buffer for reading from src
    private val sb = StringBuffer()
    private var srcEmpty = false //set to true once I read last data from src:Reader

    private var pos = 0 //position of 'parser head' from start of src
    private var shift = 0 //how much was deleted from left of buffer
    //(pos-shift) = position of 'parser head' in sb
    // - shift comes from shifting the 'window' along, instead of just growing it
    // -- also, in js and in bash : that method removes from start

    private inline val posSb get() = pos - shift

    //as parser progresses: pos ONLY increases
    // -- decreased == went back == backtracking

    private var row = 0 //how many \n seen in src before pos          --- dubs row
    private var col = 0 //#of chars from nearest \n|startOfSrc to pos --- dubs column

    //if I want to prevent full search, and instead limit bugger
    // - can turn into ~LL(c) by only keeping some size of buffer
    // -- after some point I 'commited' to the selected branch
    // --- then errors will be thrown as if I
    // - but that is optimization; first I need proof of concept, that it works at all
    // -- same with char 'array map switch' for Alt
    // - THEN: Include arg Int, max number of characters for backtracking
    // --- btw. it will be impossible to read regexes longer than this
    // - FOR NOW: NO BUFFER DELETING

    // DO NOT STORE SHIFT -- shift cannot be undone
    fun pin() = Pin(pos, row, col, this)

    class CannotBacktrack(str: String) : IllegalStateException(str)

    fun reset(pin: Pin) {
        if ((pin.pos - shift) < 0) {
            throw CannotBacktrack(
                "buffer no longer available. (required:${pin.pos}, minimal:$shift); buffer:${
                substring(0, posSb)}"
            )
            //TODO: better buffer substring: make it until FURTHEST failure yet
            // -- all of that must have matched, before backtracked all the way here
            // - problem only with very long regexes, but it's STATED that regex cannot match more than backtracking N
        }

        pos = pin.pos
        row = pin.row
        col = pin.col
    }

    //-1 == unprotected
    // - otherwise: any auto-buffer-delete CANNOT delete before and including this pos
    private var protectPosFromDelete = 0

    private inline fun <T> protectingPos(p: Int = pos, body: () -> T): T {
        val outer = protectPosFromDelete
        try {
            protectPosFromDelete = p
            return body()
        } finally {
            protectPosFromDelete = outer
        }
    }

    private tailrec fun nom(): Boolean {
        // read more into buffer
        // ret==read anything

        if (srcEmpty) return false

        val lenRead = src.read(cbuf)
        if (lenRead < 0) {
            srcEmpty = true
            return false
        } else {
            sb.append(cbuf, 0, lenRead)
        }

        if (lenRead == 0) //must either READ or END
            return nom()

        return true
    }

    @Suppress("NOTHING_TO_INLINE")
    private inline fun moveCore(by: Int): Boolean {
        pos += by
        while (posSb >= sb.length) {
            if (!nom())
                return false
        }
        return true
    }

    fun move(by: Int): Boolean {
        //check (by>=0) ... is it needed?
        // - could underflow buffer... yeah...
        // - NEEDED for row,col computing;; only backtracking can move back
//        if (by + posSb < 0)
//            throw IndexOutOfBoundsException("pos:$pos shift:$shift by:$by")
        if (by <= 0)
            throw IllegalArgumentException("must be (>0); is: by:$by")

        //in future, nom will also be able to delete? ... NO

        //count #of \n in (pos, pos+by)
        //

        //actually, this protecting should not be needed
        // - I CAN NEVER DELETE what is under pos.... obviously
        // OOOH! . but it ISN'T -- this has moved position, by the time nom is called
        // OK, makes sense! nice ^^
        protectingPos {
            val start = posSb
            var end = posSb + by //NOT INCLUSIVE (for search);; by is len
            return moveCore(by).also {
                if (!it) //end of Reader:: do not read after end
                    end = sb.length

                var ncount = 0 //#of seen \n-s
                var npos = -1 //last seen \n
                //UNTIL: do not process the current pos yet
                // - included at the start of this loop (next time)
                for (i in start until end) {
                    if (sb[i] == '\n') {
                        ncount++
                        npos = i
                    }
                }
                row += ncount
                if (npos >= 0) {
                    //seen \n -- col is len from that
                    col = (end - npos)
                } else {
                    //not seen \n -- just add to current col
                    col += (end - start)
                }
            }
        }
    }

    fun informFailedBranch(s: Stack) {
        //TODO: store last N, providing them as 'what went wrong' information
        // - better: N that went furthest, maybe?
        //for now, for testing: just show right away
        println("failed: $s IS: ${substring(0, 10).replace("\n", "\\n")}")
    }

    fun informParsedTopLevel() {
        //trim buffer: before 'here' is not needed anymore - already fully parsed
        ltrimSb(0)
    }

    private fun ltrimSb(untilPosRelative: Int) {
        val u = posSb + untilPosRelative //exclusive shift end
        //will delete buffer up to (posSb+arg)
        //out of range values are handled as maximum in that direction

        when {
            u < 0 -> return //buffer already shorter
            u > posSb -> throw IllegalArgumentException("ltrimSb: only (<=0) arguments allowed")
            else -> {
                sb.delete(0, u) //end is exclusive
                shift += u
            }
        }
    }

    private fun ensureLookahead(pp: Int) {
        //pp is position in buffer
        if (pp < 0)
            return //for now: always fine, and getc will just return 0

        while (pp >= sb.length) {
            if (!nom()) {
                if (pp - 100 > sb.length) //only throw when out by a lot
                    throw UnexpectedEofInLookahead()
                else return
            }
        }
    }

    override val length: Int //seems to work
        get() = /*if (srcEmpty) sb.length - pos else*/ (Int.MAX_VALUE - 1000)

    override fun get(index: Int): Char {
        val i = posSb + index
        ensureLookahead(i)
        return getc(i)
    }

    private fun getc(i: Int) = if (i >= 0 && i < sb.length) sb[i] else Char.MIN_VALUE

    override fun subSequence(startIndex: Int, endIndex: Int): CharSequence {
        val ia = posSb + startIndex
        val ie = posSb + endIndex
        ensureLookahead(ie - 1)

        if (ie >= sb.length) {
            //cannot use normal: sb is shorter
            // -- this should not be problem: shold only happen at the end
            return buildString {
                for (i in ia until ie)
                    append(getc(i))
            }
        }
        //can use normal, fast
        return sb.subSequence(ia, ie)
    }

    class UnexpectedEofInLookahead : Exception()
}
//part of it is 'character enumerator'
// - can move (set new current) and always provide current

class Frame(val args: MutableList<Any>? = null, val pin: Pin? = null)

val frameEmpty = Frame()

class LeftResult(
    //for both: order must stay the same as was in alts

    //those start with a term
    val termParsers: List<IParser>,
    //those do not have start: must compose at the top
    //WRONG!!!! - needs to be a list of suffix0 chains... right?
    // outer list: alts; inner list: suffix-chain
    val suffixes: List<List<Suffix>>
) {
    fun isExactly(p: IParser) = suffixes.isEmpty() && termParsers.singleOrNull() == p

    companion object {
        fun exact(p: IParser) = LeftResult(listOf(p), emptyList())
        fun suffix0(p: Suffix) = LeftResult(emptyList(), listOf(listOf(p)))
    }
}

var dbg_uniq = 0
val dbgUniqNext get() = dbg_uniq++

inline fun <T> protectRecur(p: IParser, vs: MutableSet<IParser>, none: () -> T, body: () -> T): T {
    return if (vs.contains(p))
        none()
    else try {
        vs += p
        body()
    } finally {
        vs -= p
    }
}

inline fun <T> protectRecurSame(p: IParser, vs: MutableSet<IParser>, body: () -> T) = protectRecur(p, vs, body, body)


fun compileParser(root: IParser): IParser {
    val vertices = mutableSetOf<IParser>()
    root.collectAllVertices(vertices)

    root.printAll()
    println("^--------START")

    val vertPass2 = mutableListOf<IParser>()
    for (v in vertices) {
        if (v != v.reduceSelfPass1())
            vertPass2.add(v)
    }
    for (v2 in vertPass2) {
        v2.reduceSelfPass2()
    }

    root.printAll()
    println("^--------REDUCED")

    //TODO: maybe: simplify wold KEEP vars
    // -- then inline would inline all it can...
    // var == Alt(1)
    // SPLITTING ONLY if needed: I think doing it once works well enough?
    // ... or, mayb I cold just simplify twice...
    // ---- or even crazier: simplify until no change

    val root = root.simplify(mutableSetOf())

    root.printAll()
    println("^--------SIMPLIFIED")

    return root
}

interface Named {
    val name: String
}

interface IParser : Named {

    //...can do infinite loop? ... could return to chain...
    // - but if chain is it's own first arg: that will be infinite anyway...
    // ... hmm... maybe one day
    // --- may actually se second in alt, and so not inherently infinite...
    // -- ok, needs that.
    // -- how come splitLeftRecursionBy didn't? ... it probably did, and I just made a mistake
    // - will SPLIT such that
    fun divideLeft(p: IParser, parents: MutableSet<IParser>): LeftResult

    //implementor must: add self, and all nodes it points to (calling this on them)
    // - AND if it is in set already, just return: handle loops
    fun collectAllVertices(ns: MutableSet<IParser>)

    fun reduceSelfPass1() = this
    //called on those, who did not return itself in pass 1
    fun reduceSelfPass2() {}

    fun print(depth: Int, ns: MutableSet<IParser>)
    fun printAll() {
        val vs = mutableSetOf<IParser>()
        collectAllVertices(vs)
        for (v in vs)
            println(v.printSelf())
    }

    fun printSelf(): String
    fun printRef(): String

    fun simplify(vs: MutableSet<IParser>) = this

    /*
    either returns parsed subtree
    or returns null == branch failed
    - if returns null, should also set some error on context
    -- that is important if it was the last path, and the error will be thrown
    -- even better: should keep track exactly where in stack of rules it is
    --- so, ideally: have some list
    * */
    fun parse(f: Frame, yy: Context, stackUp: Stack): Any?

    //fun prepareFrame(): Frame

    //TODO: optimization: ask
    //fun frameArgsMaxSize(): Int = 0

    val isTerminal get() = false
}

//typed are parser DEFINITIONS
// - IParser is used internally
// -- always assured to conform  to the types: no checking, but casted for typed wrap fns
sealed class Parser<T : Any> : IParser {
    //abstract override val name: String
}

class Terminal<T : Any>(
    private val name_: String?,
    private val rgx: Regex, // BEWARE!!! - must be prepared to ONLY match from start - otherwise, broken
    internal val wrap: (Pin, MatchResult) -> T
) : Parser<T>() {
    override val name: String get() = name_ ?: "\"${rgx.pattern.substring(2)}\""

    override val isTerminal: Boolean get() = true

    override fun collectAllVertices(ns: MutableSet<IParser>) {
        ns += this
    }

    override fun divideLeft(p: IParser, parents: MutableSet<IParser>): LeftResult = LeftResult.exact(this)

    //@Language("RegExp", "", "") -- intellij highlighting
    init {
        if (!rgx.pattern.startsWith("\\A"))
            throw IllegalArgumentException("Parser.Terminal - regex MUST start with \\A")
    }

    override fun print(depth: Int, ns: MutableSet<IParser>) {
        print(name)
    }

    override fun printSelf() = "(term $name = \"${rgx.pattern.substring(2)}\")"
    override fun printRef() = name

    //override fun prepareFrame() = frameEmpty

    //ON FAILURE: keeps yy position where it was at the start
    override fun parse(f: Frame, yy: Context, stackUp: Stack): Any? = try {
        //println("term ${name} @ ${yy.substring(0, 2)}")
        val pin = yy.pin()
        rgx.find(yy)?.let {

            //println("match ${name} = ${it.value}")
            println("match: ${Stack(stackUp, this, pin)}  == \"${it.value}\"")
            val ret = wrap(pin, it)

            //matchResult has ref to yy: must not change, until processed
            // - hence move must be after wrap
            yy.move(it.value.length)
            ret
        }.also {
            if (it == null) {
                yy.informFailedBranch(Stack(stackUp, this, pin))
            }

        }
    } catch (e: Context.UnexpectedEofInLookahead) {
        //TODO: SET err on yy
        null
    }
}


class Suffix(
    override val name: String, //from what chain is it derived
    val ps: List<IParser>,
    //takes ONE MORE ARGS than ps.size
    // - first extra ergument //is just a suffix0
    val wrapPlus: (Pin, List<Any>) -> Any
) : Named {
    fun print(depth: Int, ns: MutableSet<IParser>) {
        print("($name--")
        for (p in ps) {
            print(" ")
            p.print(depth + 1, ns)
        }
        print(")")
    }

    fun simplify(vs: MutableSet<IParser>) = ps.map { it.simplify(vs) }.let {
        if (eqList(ps, it))
            this
        else
            Suffix(name, it, wrapPlus)
    }
}

class Chain<T : Any>(
    override val name: String,
    private val ps: List<IParser>,
    private val wrap: (Pin, List<Any>) -> T
) : Parser<T>() {
    //all must match

    //name is UNDER what alt it was defined
    // - they do not have their own names, but alts will move around during compilation
    // - for errors reporting: good to know

    //override fun prepareFrame(): Frame = Frame(arrayOfNulls(frameArgsMaxSize()))
    //override fun frameArgsMaxSize(): Int = ps.size

    override fun collectAllVertices(ns: MutableSet<IParser>) {
        if (ns.contains(this))
            return

        ns += this
        for (p in ps) {
            p.collectAllVertices(ns)
        }
    }

    private val suffix get() = Suffix(name, ps.drop(1), wrap)

    override fun divideLeft(p: IParser, parents: MutableSet<IParser>) =
        //maybe exact? maybe empty?
        protectRecur<LeftResult>(this, parents, { throw IllegalStateException("weird loop to self : $name") }) {
            //if empty ps : will always match -- can just return myself
            if (ps.isEmpty())
                return LeftResult.exact(this)

            //otherwise, I have some first, so divide it, and if needed, replace myself

            if (ps[0] == p)
                return LeftResult.suffix0(suffix)

            val r = ps[0].divideLeft(p, parents)

            if (r.isExactly(ps[0]))
                return LeftResult.exact(this)

            return LeftResult(
                r.termParsers.map { this.replaceFirst(it) },
                r.suffixes.map { it + suffix }
            )
        }

    override fun print(depth: Int, ns: MutableSet<IParser>) {
        protectRecur(this, ns, { print("@-$name") }) {
            print("($name-:")
            for (p in ps) {
                print(" ")
                p.print(depth + 1, ns)
            }
            print(")")
        }
    }

    override fun printSelf() = "(all $name = ${ps.joinToString(" ") { it.printRef() }})"
    override fun printRef() = name

//    fun parse2(f: Frame, yy: Context): Any? {
//        println("chain $name @ ${yy.substring(0, 2)}")
//        val pin = f.pin ?: yy.pin()
//
//        if (ps.isEmpty()) {
//            return wrap(pin, emptyList())
//        }
//
//        //always must be passed CLEARED
//        val args = f.args ?: mutableListOf()
//
//        //they all must match
//        for (p in ps) {
//            //cannot reuse f: all but first start elsewhere
//            val r = p.parse(frameEmpty, yy,)
//            if (r == null) {
//                yy.reset(pin)
//                return null //did not match: fail all
//            } else {
//                args.add(r)
//            }
//        }
//
//        //after all matched
//        return wrap(pin, args)
//    }

    override fun parse(f: Frame, yy: Context, stackUp: Stack): Any? {
        //println("chain $name @ ${yy.substring(0, 2)}")
        val pin = f.pin ?: yy.pin()

        val s = Stack(stackUp, this, pin)
        println("enter: $s")
        //println("enter: $s ;; ${yy.substring(0,40).replace("\n", "\\n")}")

        if (ps.isEmpty()) {
            return wrap(pin, emptyList())
        }

        //always must be passed CLEARED
        val args = f.args ?: mutableListOf()

        return if (parseAll(pin, args, ps, yy, s)) {
            wrap(pin, args) //after all matched
        } else {
            null //did not match: fail all
        }
    }

    override fun simplify(vs: MutableSet<IParser>) =
        protectRecur(this, vs, { this }) {
            Chain(name, ps.map { it.simplify(vs) }, wrap)
        }


    private fun replaceFirst(p0: IParser, nm: String = name) =
        Chain(nm, ps.mapIndexed { i, y -> if (i == 0) p0 else y }, wrap)

}

fun parseAll(
    pin: Pin,
    args: MutableList<Any>,
    ps: List<IParser>,
    yy: Context,
    stack: Stack
): Boolean {
    //they all must match
    var i = stack.chainPos ?: 0
    for (p in ps) {
        stack.chainPos = i++
        //cannot reuse f: all but first start elsewhere
        val r = p.parse(frameEmpty, yy, stack)
        if (r == null) {
            yy.reset(pin)
            stack.chainPos = null
            return false
        } else {
            args.add(r)
        }
    }

    return true
}

fun composeSuffixChain(ss: List<Suffix>) = ss.reduce { l, r ->
    Suffix(l.name + "+" + r.name, l.ps + r.ps) { pin, a ->
        //overall I KNOW that a.size MUST== l.ps.size + r.ps.size + 1
        // -- or it's bigger, beacuse it is used for all... true
        // --- it would be exact, if this was the last one to be processed
        val lv = l.wrapPlus(pin, a)
        val ma = mutableListOf(lv) //and this replaces the +1 th
        ma.addAll(a.drop(l.ps.size + 1)) //took ps+1 args; put r after lv
        r.wrapPlus(pin, ma)
    }
}


class Reducing<T : Any>(
    override val name: String,
    private var head: IParser,
    //WRONG: that chain still might need to do dispatch...
    // - so, alt is possible...
    // ... some special, that can prepend arg to ... it's arguments? ...
    // ---- can later be unified with 'prefix sharing' if multiple alts have the same
    private var repeatAlts: List<Suffix>
) : Parser<T>() {
    //must match head
    //after that: will try to match repeat as many time as possible
    // - repeat MUST NOT be used on it's own
    // --- it's wrap functions expects EXTRA FIRST ARG
    // --- I always have to provide it
    // --- initially head, then previous result

    private fun parseAnyAlt(
        hv: Any,
        pin: Pin,
        args: MutableList<Any>,
        yy: Context,
        stack: Stack
    ): Any? {
        //println("reducA $name @ ${yy.substring(0, 2)}")

        for (s in repeatAlts) {
            args.clear()
            args.add(hv)
            stack.chainPos = 1
            if (parseAll(pin, args, s.ps, yy, stack)) {
                return s.wrapPlus(pin, args)
            }
        }
        return null
    }

    override fun parse(f: Frame, yy: Context, stackUp: Stack): Any? {

        var hv = head.parse(f, yy, Stack(stackUp, this))
            ?: return null //parser already failed: nothing else needed

        //parsed head
        // now try to match suffix as many times as possible

        var pin = yy.pin()
        val args = mutableListOf<Any>()

        while (true) {
            //println("LOOP $name: acc: $hv")
            // always must try all alts in order; if any matches: I repeat
//            val sv = parseAnyAlt(hv, pin, args, yy, Stack(stackUp, this, pin))
//            if (sv == null)
//                return hv
//            else {
//                hv = sv
//            }

            //reducing:: update acc with next value
            hv = parseAnyAlt(hv, pin, args, yy, Stack(stackUp, this, pin))
                ?: return hv


            val pinOldPos = pin.pos
            pin = yy.pin()
            if (pinOldPos == pin.pos)
                throw InfiniteRecursion(
                    "in rule: $name ; input.pos: $pinOldPos ; " +
                            "alts-suffixes: ${repeatAlts.joinToString(" ") { it.name }}"
                )
        }
    }

    class InfiniteRecursion(msg: String) : Exception(msg)

    //ALSO: only rules need to be compiled... - chains are fine as they are
    // -- chains only need to be inlined later, but that is still fine...
    // --- that is mostly just a performance optimization

    override fun collectAllVertices(ns: MutableSet<IParser>) {
        if (ns.contains(this))
            return
        ns += this
        head.collectAllVertices(ns)
        for (ra in repeatAlts)
            for (p in ra.ps)
                p.collectAllVertices(ns)
    }

    override fun divideLeft(p: IParser, parents: MutableSet<IParser>) =
        //TODO: VARIANT where none and body are the same
        protectRecurSame(this, parents) {
            head.divideLeft(p, parents)
        }


    override fun print(depth: Int, ns: MutableSet<IParser>) =
        protectRecur(this, ns, { print("@*$name") }) {
            print("($name*: ")
            head.print(depth + 1, ns)
            print(" **")
            for (p in repeatAlts) {
                print(" | ")
                p.print(depth + 1, ns)
            }
            print(")")
        }

    override fun printSelf() =
        "(reducing $name = ${head.printRef()} ** ${repeatAlts.joinToString(" ")
        {
            "(alt ${it.name} = ${it.ps.joinToString(" ", transform = IParser::printRef)})"
        }})"

    override fun printRef() = name

    override fun simplify(vs: MutableSet<IParser>) =
        protectRecur<IParser>(this, vs, { this }) {
            head = head.simplify(vs)
            repeatAlts = repeatAlts.map { it.simplify(vs) }
            this
        }
}

fun <T> eqList(l: List<T>, r: List<T>): Boolean {
    if (l.size != r.size)
        return false

    return l.zip(r).all { (a, b) -> a == b }
}

@Suppress("UNCHECKED_CAST")
class Alt<T : Any>(
    override val name: String,
    private val ps: MutableList<IParser> = mutableListOf()
) : Parser<T>() {
    //any must match

    var compiledTmp: IParser? = null

    override fun collectAllVertices(ns: MutableSet<IParser>) {
        if (ns.contains(this))
            return

        ns += this
        for (p in ps) {
            p.collectAllVertices(ns)
        }
    }

    override fun divideLeft(p: IParser, parents: MutableSet<IParser>) =
        //BEWARE: when parents.contains, maybe returning exact is not correct? idk...
        protectRecur(this, parents, { LeftResult.exact(this) }) {
            val rs = ps.map { Pair(it, it.divideLeft(p, parents)) }
            if (rs.all { (x, r) -> r.isExactly(x) })
                return LeftResult.exact(this)

            return LeftResult(
                rs.map { it.second }.flatMap { it.termParsers },
                rs.map { it.second }.flatMap { it.suffixes })
        }


    override fun parse(f: Frame, yy: Context, stackUp: Stack): Any? {
        //println("alt $name @ ${yy.substring(0, 2)}")

        if (ps.isEmpty())
            return null

        val pin = yy.pin()
        val args = mutableListOf<Any>()
        val ff = Frame(args, pin)

        for (p in ps) {
            val r = p.parse(ff, yy, Stack(stackUp, this, pin)) //if fails: will itself reset pin
            if (r != null)
                return r
            else
                args.clear()
        }

        return null //no matched
    }

    override fun reduceSelfPass1(): IParser {

        val r = this.divideLeft(this, mutableSetOf())
        if (r.isExactly(this))
            return this

        val ss = r.suffixes.map { composeSuffixChain(it) }

        //_T for terminals
        val head = Alt<T>(name + "_T$dbgUniqNext", r.termParsers.toMutableList())

        if (ss.isEmpty()) {
            compiledTmp = head
            return head
        }

        //suffixes would be S, but do not haev a name on their own

        val loop = Reducing<T>(name + "_R$dbgUniqNext", head, ss)
        compiledTmp = loop

        // SOLUTION: second compilation pass: will SET myself from the tmp

        return loop
    }

    override fun reduceSelfPass2() {
        compiledTmp?.let {
            ps.clear()
            ps.add(it)
        }
    }

    private fun simplify0() = if (ps.size == 1) ps[0] else this
    override fun simplify(vs: MutableSet<IParser>) =
        protectRecur(this, vs, { simplify0() }) {
            ps.replaceAll { it.simplify(vs) }
            simplify0()
        }

    override fun print(depth: Int, ns: MutableSet<IParser>) =
        protectRecur(this, ns, { print("@|$name") }) {
            val s = ""//"\n" + " ".repeat(depth)
            val s2 = " "//""\n" + " ".repeat(depth + 2)

            print("$s($name::")
            for (p in ps) {
                print("$s2| ")
                p.print(depth + 1, ns)
            }
            print("$s2)")
        }

    override fun printSelf() = "(any $name = ${ps.joinToString(" ") { it.printRef() }})"
    override fun printRef() = name

    //user API
    private var ord = 0
    private val nameNext get() = name + "_" + ord++

    fun alt(wrap: (Pin) -> T): Alt<T> {
        ps.add(Chain(nameNext, emptyList()) { p, _ -> wrap(p) })
        return this
    }

    fun <A1 : Any> alt(p1: Parser<A1>, wrap: (Pin, A1) -> T): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1)) { p, a -> wrap(p, a[0] as A1) })
        return this
    }

    fun alt(p1: Parser<T>): Alt<T> {
        //ps.add(Chain(name, listOf(p1)) { p, a -> a[0] as T })
        ps.add(p1)
        return this
    }

    fun <A1 : Any, A2 : Any> alt(p1: Parser<A1>, p2: Parser<A2>, wrap: (Pin, A1, A2) -> T): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2)) { p, a -> wrap(p, a[0] as A1, a[1] as A2) })
        return this
    }


    fun <A1 : Any, A2 : Any, A3 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>,
        wrap: (Pin, A1, A2, A3) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3))
        { p, a -> wrap(p, a[0] as A1, a[1] as A2, a[2] as A3) })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        wrap: (Pin, A1, A2, A3, A4) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4))
        { p, a -> wrap(p, a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4) })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>, p5: Parser<A5>,
        wrap: (Pin, A1, A2, A3, A4, A5) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5))
        { p, a -> wrap(p, a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4, a[4] as A5) })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3,
                a[3] as A4, a[4] as A5, a[5] as A6, a[6] as A7
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any, A9 : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any, A9 : Any, AA : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any,
            A9 : Any, AA : Any, AB : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any,
            A9 : Any, AA : Any, AB : Any, AC : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>, pc: Parser<AC>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB, AC) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB, a[11] as AC
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any,
            A9 : Any, AA : Any, AB : Any, AC : Any, AD : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>, pc: Parser<AC>,
        pd: Parser<AD>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB, AC, AD) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB, a[11] as AC, a[12] as AD
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any,
            A9 : Any, AA : Any, AB : Any, AC : Any, AD : Any, AE : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>, pc: Parser<AC>,
        pd: Parser<AD>, pe: Parser<AE>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB, AC, AD, AE) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd, pe))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB, a[11] as AC,
                a[12] as AD, a[13] as AE
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any, A9 : Any, AA : Any, AB : Any, AC : Any, AD : Any, AE : Any, AF : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>, pc: Parser<AC>,
        pd: Parser<AD>, pe: Parser<AE>, pf: Parser<AF>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB, AC, AD, AE, AF) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd, pe, pf))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB, a[11] as AC,
                a[12] as AD, a[13] as AE, a[14] as AF
            )
        })
        return this
    }

    fun <A1 : Any, A2 : Any, A3 : Any, A4 : Any, A5 : Any, A6 : Any, A7 : Any, A8 : Any, A9 : Any, AA : Any, AB : Any, AC : Any, AD : Any, AE : Any, AF : Any, AG : Any>
            alt(
        p1: Parser<A1>, p2: Parser<A2>, p3: Parser<A3>, p4: Parser<A4>,
        p5: Parser<A5>, p6: Parser<A6>, p7: Parser<A7>, p8: Parser<A8>,
        p9: Parser<A9>, pa: Parser<AA>, pb: Parser<AB>, pc: Parser<AC>,
        pd: Parser<AD>, pe: Parser<AE>, pf: Parser<AF>, pg: Parser<AG>,
        wrap: (Pin, A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB, AC, AD, AE, AF, AG) -> T
    ): Alt<T> {
        ps.add(Chain(nameNext, listOf(p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd, pe, pf, pg))
        { p, a ->
            wrap(
                p,
                a[0] as A1, a[1] as A2, a[2] as A3, a[3] as A4,
                a[4] as A5, a[5] as A6, a[6] as A7, a[7] as A8,
                a[8] as A9, a[9] as AA, a[10] as AB, a[11] as AC,
                a[12] as AD, a[13] as AE, a[14] as AF, a[15] as AG
            )
        })
        return this
    }
}