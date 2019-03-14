#lang brag
bs-program    : (pushdata-stat | OPCODE)+
pushdata-stat : (OPPUSHDATA size HEX) | size HEX
size          : HEX | DEC