# lrb-parser
scribble phase -- (naive) Left Reducing Backtracking Parser

### Limitations
When a rule-alt does not start with left recursion (including indirect), but contains it: 
all of the terminals combined before reaching the recursive step **must not** match empty string. 
If the parser does not move before reaching left recursion, it will allow infinite loop.
As a rule of thumb: terminals that match emoty string may only be at the end of a chain.

Only reduces left recursion in first element of a chain (rule-alt)

## TODO
- error reporting
- track row,col
- improvements
- proper testing
- load yacc files (only the tree part; this has no separate lexer)
- (for me) Learn terminology of parts of grammar.

#### tmp terminology
- `rule`: top level yacc construct. Has a list of alternatives.
- `chain`: a sequence of elements
- `rule-alt`: a chain that is one of the alternatives of a rule
- `element`:  (rule reference OR lexem)
- `lexem`: something that will actually match some length of text
  - for now, a regexp
