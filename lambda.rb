ZERO = ->p {-> x {x}} # p is a proc, x is a variable
ONE = ->p {-> x {p[x]}}
TWO = ->p {-> x {p[p[x]]}}
THREE = ->p {-> x {p[p[p[x]]]}}
FIVE = -> p { -> x { p[p[p[p[p[x]]]]] } }
FIFTEEN = -> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]] } }

def to_integer(proc)
    proc[-> n {n + 1}][0]
end

puts to_integer(FIFTEEN)

TRUE  =  ->x { -> y {x}}
FALSE = ->x { -> y {y}}



IF = ->b { b }

def to_boolean(proc)
    IF[proc][true][false]
end

def zero?(proc)
    proc[-> x { FALSE }][TRUE]
end

IS_ZERO = ->n{n[->x{FALSE}][TRUE]}

PAIR = -> x { -> y { -> f { f[x][y] } } }
LEFT = -> p { p[-> x { -> y { x } } ] }
RIGHT = -> p { p[-> x { -> y { y } } ] }

my_pair = PAIR[THREE][FIVE] #->f {f[THREE][FIVE]]}

#LEFT[my_pair] = my_pair[->x {->y {x}}}] =  PAIR[THREE][FIVE][->x {->y {x}}] = THREE

INCREMENT = -> n { -> p { -> x { p[n[p][x]] } } }

#INCREMENT[TWO] = -> p { -> x { p[TWO[p][x]] } } = -> p { -> x { p[p[p[x]] } } = THREE

def slide(pair)
    [pair.last, pair.last + 1]
end

# slide(slide(slide(slide([0, 0])))) => (3,4)

SLIDE = -> p { PAIR[RIGHT[p]][INCREMENT[RIGHT[p]]] }
DECREMENT = -> n { LEFT[n[SLIDE][PAIR[ZERO][ZERO]]] }

ADD = -> m { -> n { n[INCREMENT][m] } }
SUBTRACT = -> m { -> n { n[DECREMENT][m] } }
MULTIPLY = -> m { -> n { n[ADD[m]][ZERO] } }
POWER = -> m { -> n { n[MULTIPLY[m]][ONE] } }

IS_LESS_OR_EQUAL =-> m { -> n {IS_ZERO[SUBTRACT[m][n]]} }


#MOD =-> m { -> n {IF[IS_LESS_OR_EQUAL[n][m]][-> x {MOD[SUBTRACT[m][n]][n][x]}][m]} }

Y = -> f { -> x { f[x[x]] }[-> x { f[x[x]] }] }

Z = -> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }

MOD =Z[-> f { -> m { -> n {IF[IS_LESS_OR_EQUAL[n][m]][-> x {f[SUBTRACT[m][n]][n][x]}][m]} } }]

EMPTY = PAIR[TRUE][TRUE]
UNSHIFT = -> l { -> x {PAIR[FALSE][PAIR[x][l]]} }
IS_EMPTY = LEFT
FIRST = -> l { LEFT[RIGHT[l]] }
REST = -> l { RIGHT[RIGHT[l]] }

=begin
def range(m, n)
    if m <= n
    range(m + 1, n).unshift(m)
    else
    []
    end
end
=end
RANGE =Z[-> f {-> m { -> n {IF[IS_LESS_OR_EQUAL[m][n]][-> x {UNSHIFT[f[INCREMENT[m]][n]][m][x]}][EMPTY]}}}]