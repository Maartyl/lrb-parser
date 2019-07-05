# lrb-parser
scribble phase -- (naive) Left Reducing Backtracking Parser

Order matters. Evaluation is always equivalent to trying all alternatives in order.
- The only exception is left recursion: those branches 

For now, mostly a proof of concept (it probably alraedy exists, but I haven't found it)
- The point was to be able to use LL-ish parser with left recursion.
- All nice-to-use parsers I've found could not handle left recursion. 
    - I found a way to remove it, without a need to change 'handlers' (code generating ast nodes; after rule-alt)
    - Surely nothing new. Maybe even wrong. I just haven't found it.
    
I also wanted a tiny parser, that can easily be included in projects, etc. (also portable; hence java)

If you feel I stole your idea, please tell me! I will most likely prefer to use your parser than spend time writing this. 
(but I swear I thought of it myself - not copying anything I've found)
(honestly, it's so small that it probaly doesn't merit much rights, but whatever)

### Limitations
When a rule-alt does not start with left recursion (including indirect), but contains it: 
all of the terminals combined before reaching the recursive step **must not** match empty string. 
If the parser does not move before reaching left recursion, it will allow infinite loop.
As a rule of thumb: terminals that match emoty string may only be at the end of a chain.

Only reduces left recursion in first element of a chain (rule-alt)

## TODO
- error reporting
-[x] track row,col
- general improvements
- proper testing
- providing better 'position+context' interface to handlers
- optimization/limitation: optionally Limit backtracking to N(+c) characters
    - as is right now, will keep buffer of entire top-level structure in memory
    - not a problem for parsing smaller files
    - BUT: can start parsing before full file is loaded - will load more as needed
- optimization: if an elem in chain is a chain: splice   (those not branching only)
    - last is simple; others will need some smart way to compose handlers
- optimization: somehoe merge lexemes, so I don't need to try one at a time
- load yacc files (only the tree part; this has no separate lexer)
- (for me) Learn terminology of parts of grammar.

#### tmp terminology
- `rule`: top level yacc construct. Has a list of alternatives.
- `chain`: a sequence of elements
- `rule-alt`: a chain that is one of the alternatives of a rule
- `element`:  (rule reference OR lexem)
- `bracnh`: a path through graph, either matching the input, or failing (followed by trying another branch)
- `lexem`: something that will actually match some length of text
    - for now, a regexp

... idk. those are very crude... 