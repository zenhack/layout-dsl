This file contains a formal grammar specification in EBNF. The EBNF is
lifted from the [Go Language Specification][1]. The description of that
notation is reproduced here for reference. Descriptions of other
language features that correspond are largely lifted from there as well.
The copyright notice for the original source document is:


    Copyright (c) 2009 The Go Authors. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

       * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
       * Neither the name of Google Inc. nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Notation

The syntax is specified using Extended Backus-Naur Form (EBNF):

```
Production  = production_name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = production_name | token [ "…" token ] | Group | Option |
Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

Productions are expressions constructed from terms and the following
operators, in increasing precedence:

```
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

Lower-case production names are used to identify lexical tokens.
Non-terminals are in CamelCase. Lexical tokens are enclosed in double
quotes "" or back quotes ``.

The form a … b represents the set of characters from a through b as
alternatives. The horizontal ellipsis … is also used elsewhere in the
spec to informally denote various enumerations or code snippets that are
not further specified.

# Source code representation

Source code is Unicode text encoded in UTF-8. The text is not
canonicalized, so a single accented code point is distinct from the same
character constructed from combining an accent and a letter; those are
treated as two code points. For simplicity, this document will use the
unqualified term character to refer to a Unicode code point in the
source text.

Each code point is distinct; for instance, upper and lower case letters
are different characters.

Implementation restriction: For compatibility with other tools, a
compiler may disallow the NUL character (U+0000) in the source text.

Implementation restriction: For compatibility with other tools, a
compiler may ignore a UTF-8-encoded byte order mark (U+FEFF) if it is
the first Unicode code point in the source text. A byte order mark may
be disallowed anywhere else in the source.

# Letters and digits

The underscore character _ (U+005F) is considered a letter.

```
letter        = "A" … "Z" | "a" … "z" | "_" .
binary_digit  = "0" | "1"
decimal_digit = "0" … "9" .
octal_digit   = "0" … "7" .
hex_digit     = "0" … "9" | "A" … "F" | "a" … "f" .
```

# Lexical elements

## Comments

Comments serve as program documentation. There are two forms:

* Line comments start with the character sequence `//` and stop at the
  end of the line.
* General comments start with the character sequence `/*` and stop with
  the first subsequent character sequence `*/`.

A comment cannot start inside a rune or string literal, or inside a
comment. A general comment containing no newlines acts like a space. Any
other comment acts like a newline.

TODO: the above paragraph is not (yet) meaningful for the layout-dsl,
but left here since (a) it doesn't conflict, and (b) it may make sense
if we add string/char literals and/or significant newlines at some
point.

# Identifiers

Identifiers name program entities such as variables and types. An
identifier is a sequence of one or more letters and digits. The first
character in an identifier must be a letter.

```
identifier = letter { letter | decimal_digit } .
```

# Keywords

The following keywords are reserved and may not be used as identifiers.

* layout
* struct
* type
* uint
* bool

# Integer literals

An integer literal is a sequence of digits representing an integer
constant. An optional prefix sets a non-decimal base: 0o or | 0O for octal,
0x or 0X for hexadecimal, 0b or 0B for binary. In hexadecimal literals,
letters a-f and A-F represent values 10 through 15.

```
int_lit             = decimal_lit | octal_lit | hex_lit | binary_lit .
decimal_lit         = noradix_decimal_lit | radix_decimal_lit .
noradix_decimal_lit = ( "1" … "9" ) { decimal_digit } .
radix_decimal_lit   = "0" ( "d" | "D") decimal_digit { decimal_digit } .
octal_lit           = "0" ( "o" | "O" ) octal_digit { octal_digit } .
hex_lit             = "0" ( "x" | "X" ) hex_digit { hex_digit } .
binary_lit          = "0" ( "b" | "B" ) binary_digit { binary_digit } .
```

```
42
0600
0xBadFace
170141183460469231731687303715884105727
0b11010101
```

# Constant field literals

A constant field is a sequence of digits representing a field in a data
structure with both constant width and constant value. It is of the
form `<size>'<value>`. The size is always in bits.

```
const_field = int_lit "'" int_lit
```

# Types

Types define the *logical* structure of values.

```
Type     = TypeName | TypeLit .
TypeName = identifier .
TypeLit  = StructType | UIntType | BoolType

StructType = "struct" "{" { IdentifierList ":" Type } "}" .
UIntType   = "uint" "<" int_lit ">" .
BoolType   = "bool" .

IdentifierList = identifier { "," identifier } .
```

# Layouts

Layouts define the *physical* structure of values. Layouts consist of a set of
optional annotations specifying things like endianness and alignment, and a
physical mapping of fields in the corresponding type declaration.

```
Layout       = [ "(" [ AnnotationList ] ")" ] LayoutField .
LayoutField  = identifier [ LayoutSlice | LayoutStruct ] .
LayoutStruct = "{" { Layout } "}" .
LayoutSlice  = "[" int_lit ":" int_lit "]" .

AnnoationList = Annotation { "," Annotation } .
Anotation     = "big" | "little"  | "align" "=" int_lit .
```
