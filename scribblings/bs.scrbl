#lang scribble/manual
@(require (for-label bs))

@title{Script: Testing Bitcoin Riddles}
@author{Yuhao Zhang}

@defmodulelang[bs]

@hyperlink["https://en.bitcoin.it/wiki/Script"]{Script} or (Bitcoin Script)
is a language used by Bitcoin's scripting system. It is a stack based

Some opcodes needs meta data from @hyperlink["https://en.bitcoin.it/wiki/Transaction"]{transactions}
in order to operate, like @tt{OP_CHECKSIG} and @tt{OP_MULTICHECKSIG}, etc. These opcodes
are not supported by current version of @racketmodname[bs].
