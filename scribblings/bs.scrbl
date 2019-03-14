#lang scribble/manual
@(require scribble/bnf
          (for-label bs))

@title{Script: Testing Bitcoin Riddles}
@author{Yuhao Zhang}

@defmodulelang[bs]

@hyperlink["https://en.bitcoin.it/wiki/Script"]{Script} or (Bitcoin Script)
is a language used by Bitcoin's scripting system.
It is a stack based language read from left to right. 

Some opcodes needs meta data from @hyperlink["https://en.bitcoin.it/wiki/Transaction"]{transactions}
in order to operate, like @tt{OP_CHECKSIG} and @tt{OP_MULTICHECKSIG}, etc. These opcodes
are not supported by current version of @racketmodname[bs].

@section{Basic Syntax}

A @italic{script} is composed by a series of @italic{opcodes} and @italic{pushdata statements}.
Both opcodes and pushdata statements are used to manipulate datas stored in a @italic{script machine}.

@subsection{Opcodes's Operand: Script Machine}

A @emph{script machine} is a data structure for storing current script states
@defstruct*[s-machine ([main-stk stack?]
                       [alt-stk stack?]
                       [tran-state boolean?] [level (listof boolean?)])]{
 A script machine is a data structure that stores the states
 of current running script program. It has a main stack @tt{main-stk},
 an alternative stack @tt{alt-stk}, a current transaction state @tt{tran-state}
 and a list @tt{level} representing the current if-block level.

 When @tt{tran-state} is being marked as @racket[#f],
 Script will stop further execution and report invalid transaction.
}

@subsection{Pushdata Statement}
A @emph{pushdata statement} is used to push data onto the main stack of script machine.
It needs to specify the size of data being pushed, and the data itself in hexadecimal
format (e.g. @tt{0x0102}).

For example, to push hexadecimal data @tt{0x01020304} onto the main stack, we write

@verbatim{4 0x01020304}

In the above example, the data size specifier is written in decimal.
It can also be written in hexadecimal format, like

@verbatim{0x04 0x01020304}

You can also specify your pushdata statements with a special opcode: one from
@tt{OP_PUSHDATA1}, @tt{OP_PUSHDATA2}, @tt{OP_PUSHDATA3} and @tt{OP_PUSHDATA4}.

@verbatim{OP_PUSHDATA1 2 0x0102}

@codeblock|{
  OP_PUSHDATA1 2 0x0102
}|

The number followed after @tt{OP_PUSHDATA} indicates the size specifiers's size
upper limit. For instance, with @tt{OP_PUSHDATA2} you can only specify a data with size not
larger than @math{256^2 - 1 = 65535} bytes, which also means your data size specifier
should be an integer range between 0 and 65535.

@subsection{Opcodes}

Opcodes are procedures manipulating datas on the script machine.

@subsection{Summary: Grammar of Script}
@BNF[(list @nonterm{program}
           @BNF-seq[@kleenestar[@nonterm{whitespace}]
                    @nonterm{expression}
                    @kleenestar[@BNF-group[@kleeneplus[@nonterm{whitespace}]
                                           @nonterm{expression}]]])
     (list @nonterm{expression}
           @BNF-alt[@nonterm{opcode} @nonterm{pushdata-statement}])
     (list @nonterm{pushdata-statement}
           @BNF-seq[@litchar{pushdata-op} @nonterm{size} @nonterm{hex}]
           @BNF-seq[@nonterm{size} @nonterm{hex}])
     (list @nonterm{pushdata-op}
           @BNF-alt[@litchar{OP_PUSHDATA1} @litchar{OP_PUSHDATA2}]
           @BNF-alt[@litchar{OP_PUSHDATA3} @litchar{OP_PUSHDATA4}])
     (list @nonterm{size}
           @BNF-alt[@nonterm{hex} @nonterm{dec}])
     (list @nonterm{opcode}
           @BNF-alt[@litchar{OP_ADD} @litchar{OP_SUB} @BNF-etc])
     (list @nonterm{whitespace}
           @BNF-alt[@litchar{␣} @litchar{\n}])
     (list @nonterm{hex}
           @BNF-seq[@litchar{0x}
                    @kleeneplus[@BNF-group[@nonterm{hex-symbol} @nonterm{hex-symbol}]]])
     (list @nonterm{dec}
           @kleeneplus[@nonterm{digit}])
     (list @nonterm{hex-symbol}
           @BNF-alt[@nonterm{digit} @nonterm{hex-alphabet}])
     (list @nonterm{digit}
           @BNF-alt[@litchar{0} @litchar{1} @litchar{2} @litchar{3} @litchar{4}]
           @BNF-alt[@litchar{5} @litchar{6} @litchar{7} @litchar{8} @litchar{9}])
     (list @nonterm{hex-alphabet}
           @BNF-alt[@litchar{A} @litchar{B} @litchar{C} @litchar{D} @litchar{E} @litchar{F}]
           @BNF-alt[@litchar{a} @litchar{b} @litchar{c} @litchar{d} @litchar{e} @litchar{f}])]