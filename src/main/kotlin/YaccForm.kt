import cz.maa.parser.Factory
import cz.maa.parser.Parser
import java.io.FileReader
import java.io.Reader
import java.io.StringReader
import kotlin.reflect.KProperty


object YF : Factory() {

    private val testInput = """
    a 
        : a '[+-*/]' b  
        | '[0-9]+'   { fine {{} lol {}}{{ } { }  {{} }}}
        ;
    b
        : a         ;; whoa
        | '[a-z]'   
        ;
        
    """.trimIndent()

    ///home/maartyl/dev/IdeaProjects/dotScriptJava1/TEST.txt

    val ti2 = """
        input
        :  q n5 AA a_88 a f ahshlafj asdlfjk adkf adjfk adfs
                     
        | input line
;

line:
          '\n'
        | exp '\n'   { printf ("\t%.10g\n", ${'$'}1); }
        | error '\n' { yyerrok;                  }
;

exp:      NUM                { ${'$'}${'$'} = ${'$'}1;                         }
        | VAR                { ${'$'}${'$'} = ${'$'}1->value.var;              }
        | VAR '=' exp        { ${'$'}${'$'} = ${'$'}3; ${'$'}1->value.var = ${'$'}3;     }
        | FNCT '(' exp ')'   { ${'$'}${'$'} = (*(${'$'}1->value.fnctptr))(${'$'}3); }
        | exp '+' exp        { ${'$'}${'$'} = ${'$'}1 + ${'$'}3;                    }
        | exp '-' exp        { ${'$'}${'$'} = ${'$'}1 - ${'$'}3;                    }
        | exp '*' exp        { ${'$'}${'$'} = ${'$'}1 * ${'$'}3;                    }
        | exp '/' exp        { ${'$'}${'$'} = ${'$'}1 / ${'$'}3;                    }
        | '-' exp  prec NEG { ${'$'}${'$'} = -${'$'}2;                        }
        | exp '^' exp        { ${'$'}${'$'} = pow (${'$'}1, ${'$'}3);               }
        | '(' exp ')'        { ${'$'}${'$'} = ${'$'}2;                         }
;
    """.trimIndent()

    @JvmStatic
    fun main(args: Array<String>) {
        //println(testInput.substring(0, 17))
        //println(testInput.substring(17, 40))
        if (args.isEmpty()) {
            StringReader(ti2).use(::parsePrint)
        } else {
            FileReader(args[0]).use(::parsePrint)
        }
    }

    private fun parsePrint(r: Reader) {
        val p = lrb(grammar())
        val l = p.parseAll(r).toList()
        if (l.isEmpty()){
            println("nothing matched")
        }

        for (d in l) {
            d.print()
        }
    }

    fun grammar(): Parser<Rule> {
        //beware: MATCHES EMPTY //optional whitespce
        val ows = term("""([ \t\r\n]|;;([^\n]*)\n)*""", "ows")
        val ref = term("""[\p{Alpha}_][\p{Alnum}_]*""", "ref")
        val str = term("""['](([^'\\]|\\.)*)[']""", "str") { _, m -> m.groups[1]?.value ?: "!!!" }

        fun <T : Any> Parser<T>.rtrim() =
            rule<T>("anon_${this.name}_ows").alt(this, ows) { _, a, _ -> a }

        val rule by R<Rule>()
        val chain by R<List<Elem>>()
        val chainTop by R<Chain>()

        val elem by R<Elem>()
        val body by R<List<Chain>>()
        //val bodyTop by R<Body>()

        //for now only temporary
        // - to parse yacc grammar, need to handle {...} blocks
        // - IGNORED for now
        val codeYacc by R<String>()
        val codeYaccInner by R<String>()

        rule
            .alt(ows, ref.rtrim(), term(":").rtrim(), body, term(";"))
            { _, _, name, _, b, _ -> Rule(name, Body(b)) }

        chain
            .alt(elem)
            { _, s -> listOf(s) }
            .alt(chain, ows, elem)
            { _, a, _, e -> a + e }

        //FUTURE will include optional {fn}
        chainTop.alt(chain.rtrim(), codeYacc) { _, a, c -> Chain(a,c) }
        //chainTop.alt(chain.rtrim()) { _, a -> Chain(a) }
        //for now: any nested {}

        // - a? mst not be in a loop, but it's own thingy... yes
        codeYacc/// .... hmm... how to make it skipable?
            .alt(term("[{]"), codeYaccInner.opt(""), term("[}]")) { _, _, s, _ -> s }
            //.alt(term("[{][}]"))
            .alt(ows)// ACTUALLY FINE ///thought: ILLEGAL: would match EMPTY ... - infinite loop
        val nocpar = term("[^{}]+")
        codeYaccInner
            .alt(term("[{]"), codeYaccInner, term("[}]")) { _, a, b, c -> a+b+c }
            //MAYBE problem: if left matches NOTHING ... becomes LEFT RECURSIVE ???
            // .... but for left to match nothing, next must be [{}] ...
            // ,... yes, I think {}} will create infinite recursion: let's try
            // YES IT HAS
            //.alt(term("")) //this the casethat is problematic:maybe will work now?
            // ... no, now it ALWAYS matches;; ok, attempt: change to + on first only
            //.alt(term("[^{}]+"), codeYaccInner, term("[^{}]*")){_,a,b,c->a+b+c}
            .alt(nocpar)
            .alt(term("[{][}]"))
            .alt(codeYaccInner, codeYaccInner){_,a,b->a+b}


        // ... WHY problem? ... where would the loop be? - WORKS!
        // - if not LEFT recursion part...


        //bodyTop.alt(body) { _, a -> Body(a) }
        body
            .alt(chainTop, ows)
            { _, x, _ -> listOf(x) }
            .alt(body, term("\\|").rtrim(), chainTop.rtrim())
            { _, a, _, e -> a + e }

        elem
            .alt(str) { _, a -> Rgx(a) }
            .alt(ref) { _, a -> Ref(a) }


        return rule.rtrim()
    }

    //fun pos(Pin):Pos -- FUTURE

}

//FUTURE: row, col
//class Pos()

class Rule(val name: String, val body: Body) {
    fun print() {
        println("$name:")
        body.print()
    }
}

class Body(val alts: List<Chain>) {
    fun print() {
        for (a in alts) {
            print("  |")
            a.print()
            println()
        }

    }
}

data class Chain(val elems: List<Elem>, val code:String) {
    fun print() {
        for (e in elems) {
            print(" ")
            print(e)
        }
        print(" ")
        print("{$code}")
    }
}

//FUTURE: handler name
sealed class Elem {
    override fun toString() = when {
        this is Rgx -> "/${this.rgx}/"
        this is Ref -> this.name
        else -> "?${this.javaClass.name}?"
    }

}

class Rgx(val rgx: String) : Elem()
class Ref(val name: String) : Elem()