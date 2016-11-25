DSL for describing data layouts. Still in the conceptualization phase;
there is no code whatsoever yet. This document is Apache-2.0 licensed,
but future versions/implementations may be under different licenses
(definitely FOSS); we'll play it by ear.

# The Problem

When doing low-level programming -- writing hardware drivers,
implementing networking protocols, binary file formats, and so
forth, you often need to deal with some structures that don't
nicely map to a construct in your programming language. Examples:

* The x86 GDT entries.
* ACPI tables.
* IP/Tcp packets

For the GDT entry, [Zero][1] defines the following struct:

    struct GDTEnt {
        uint16_t limit_low;
        uint16_t base_low;
        uint8_t base_mid;
        uint8_t access;
        uint8_t flags_limit_high;
        uint8_t base_high;
    }__attribute__((packed));

Note the following:

* We need `__attribute__((packed))`, because otherwise C may introduce
  unwanted space between some of the fields
* We have conceptually-single fields spread across multiple locations
  in the struct; `base` spans three different struct fields.
* Some fields don't divide evenly into bytes, so they're awkwardly
  sharing variables. `flags_limit_high` is conceptually a four-bit
  flags field, and a four-bit chunk of the limit.
* Some fields are smaller than a single byte; both flags and access are
  made up of smaller bit fields.
* Sometimes we *do* want padding, but not the way the compiler will do
  it. For example, two of the bits in the flags field are supposed to
  just be zero, and one of the bits in access must always be 1.

Working with these structures is a bit awkward.

The trouble is that data definitions in C, and almost every other
programming language, really aren't designed to specify such awkward
structures. They simply can't express many of the patterns you see in
this space.

C's data types are also also somewhat oriented towards making the
compiler's job easier; generating machine code that splits a logical
value across three different locations isn't quite as trivial as storing
a value in a word-aligned memory location.

Rust doesn't even define the memory representation for its types, and
with good reasons. [See this section of the Rustonomicon][2] for
details.

..Though note that you can impose layout restrictions like those in C,
if needed.

Finally, as described in the Rustonomicon, using packed structs like the
above can be a little risky, since it opens up the possibility of doing
unaligned loads and stores.

# Proposed solution

A DSL for defining bit-level data layouts in an expressive way. Tools
can be written to derive getters and setters for these types, or to
generate constant values to embed in executables, or potentially other
applications.

## Overview

For each data type, we define two things:

1. The logical structure of the data type
2. Its physical layout.

Example:

    /* Declares a logical view of the data; base is one conceputal
     * field, so we express it that way.
    type GDTEnt struct {
        // The `unit` type can be parametrized over any bit length:
        base: uint<32>
        limit: uint<20>

        flags: struct {
            gr, sz: bool
        }

        access: struct {
            ac, rw, dc, ex, pr: bool
            privl: uint<2>
        }
    }

    /* Declares the physical layout of the data. The whole thing is
     * declared to be little endian; this is inherited by component
     * fields unless they specifically override it (see the section on
     * endianness, below).
     *
     * Endianness is unspecified by default, but if left unspecified
     * may only be used as part of a larger structure.
     */
    layout(little) GDTEnt {
        // Denotes the bottom 16 bits of the limit field. Can be specified as
        // either [hi:lo] or [lo:hi]. We allow both to make transcribing
        // from hardware manuals easier.
        limit[15:0]

        base[23:0]

        access {
            // Without the slice notation, we embed the whole field.
            // Booleans are assumed 1 bit.
            ac, rw, dc, ex

            // A bit that is always 1. Syntax is Verilog inspired, of
            // the form <length>'<radix><value>. The radix `b` is
            // base 2.
            1'b1

            privl // 2 bits wide; derived from the type declaration.
            pr
        }

        limit[19:16]
        flags {
            2'b0 // 2 bit field with the value 0.
            sz
            gr
        }
        base[24:32]
    }

A tool could then be used to generate C code that could be called like
so:

    GDTEnt_set_base(&ent, 0xffffffff); // set the value of the `base` field.
    uint32_t lim = GDTEnt_get_limit(&ent); // get the value of the `limit` field.

Or in a language that has more a bit more powerful mechanisms for
abstraction, such as C++ or rust:

    ent.base = 0xffffffff;
    uint32_t lim = ent.limit;

## Endianness

Endianness can be declared explicitly on an entire layout, or on any field,
and is inherited by sub-components unless they specifically override it.
As an example, suppose we have some sort of packet, with big-endian
headers and trailers, but whose payload is little endian. We might have a
layout like this:

    layout(big) Packet {
        type[0:4]
        0'b4
        total_len
        src_addr
        dest_addr
        // other header fields
        (little) payload {
            foo[0:16]
            bar[0:32]
            3'b0
            // ...
        }
        // trailer fields
        trailer1[0:4]
        trailer2[0:8]
    }

A layout is permitted to omit endianness information at the top level,
e.g:

    layout Foo {
        bar
        baz[0:3]
        0'x3f
    }

In this case, the data type can only be used as part of a larger
structure that *does* define the overall endianness.

## Alignment

Alignment could be specified with an annotation similar to that used for
endianness, e.g:

    layout(little,align=8B) Foo {
        // ...
    }

Would denote a little-endian structure that must be 8-byte aligned.

Tools should detect inconsistencies in alignment specifications and
report them as errors, e.g:


    type foo {
        x: bar
    }

    type bar {
        y: bool
    }

    layout(align=4) foo {
        x
    }

    layout(align=8) bar {
        y 63'b0
    }

In this case, foo will be aligned on a 4-byte boundary, but one of it's
members (x) demands an 8-byte alignment.

## Recommendations for code-generation tools

The most obvious class of tool that uses this language is one which
generates data types and getters/setters for a programming language,
such as C, C++, or Rust. This section lists some general recommendations
for these tools.

### Setters should check inputs

In many cases, the host language will not be able to capture the
required constraints in its type system. For example, C does not have a
2-bit unsigned integer type, so a setter for the `privl` field in the
`GDTEnt` structure defined above could be passed values that don't
actually fit in the field.

Setters should check inputs and panic/abort if they are invalid. In some
contexts this may a problem for performance, in which case it is
acceptable to generate `*_unsafe` variants, but APIs should discourage
their use.

# Open Questions

## Sum types?
If we have a data structure like (OCaml syntax):

    type t =
        | Foo of (uint32, uint64)
        | Bar of bool

A typical approach to laying this out in memory is to have something
similar to this C declaration:

    struct T {
        int tag;
        union {
            struct {
                uint32_t x;
                uint64_t y;
            } foo;
            struct {
                bool x;
            } bar;
        };
    };

I've seen similar structures (I think there's one that shows up in one
of the APIC related tables?), but that have the "tag" in some weird spot
in the middle of the data structure. I want to be able to express this
sort of thing as well.

One stab, which is more verbose than I want:

    type TTag enum(uint) {
        foo = 1
        bar = 7
    }

    type T union(TTag) {
        foo: struct {
            x: u32
            y: u64
        }
        bar: struct {
            x: bool
        }
    }

    layout T {
        value[0:56]
        tag[0:2]
        value[56:96]
        tag[2:3]
    }

Also, this fails to capture more tricky cases like the mips instructions
described below.

## Pointers/References?

* Relative vs absolute?
* Physical/virtual address distinction?
* "Far" pointers? Places this shows up:
  * real mode x86
  * Cap'N Proto

I see two ways of approaching this:

* Mostly punt; keep this somewhat impoverished
* Try to come up with a way to compose things. We're not going to
  capture every possible addressing model by just enumerating them.

## MMIO/other constrained access methods.

MMIO structures tend to have requirements about the load/store sizes.
The language should be able to capture this, and tools should generate
code that respects this.

There are also other cases where data must be accessed in particular
ways, e.g. port IO on x86, or filesystems stored on disk.

It may make sense to have field access constraints defined as a third
facet, alongside logical and physical layout.

For now, the "default field access definition" assumes memory
with specified alignments; portio and other weirder things are left
as future work.

## Variable length fields?

## Scope?

I'm pretty sure I want at least the sum-type functionality, but the
pointer stuff gets a little trickier, and you could go pretty deep with
this. Need to better define the scope of things we want to deal with.

## What code to gen?

* What should the exposed APIs look like? What tools would be useful? We
  have some notions described above, but should keep thinking on this.

Thoughts:

* Would be nice if we could statically embed complex values. In Zero we
  do some hackery with C macros to have the final GDT embedded in the
  executable, so there's no run-time construction. In general many data
  structures are too difficult to express for this.

## Things to look into

* Read about various things like:
  * Filesystems
  * Network protocols
  * Hardware (pick through intel/arm manuals, maybe ppc and some more
    obscure architectures)
  * Instruction set encodings?
    * Mips includes some interesting challenges; See below. Probably
      similar examples in places I'm less familiar with.
    * This very well may be out of scope.
* See what prior art exists.

## Mips encoding

Mips has three basic encoding types: R, I, and J. In all cases, the
first six bits are an opcode. For R-Type instructions, the opcode field
is zero, and there is a secondary 'funct' field elsewhere in the
instruction. The I and J types only have one opcode.

The naive sum type solution above doesn't work here, since you don't
have two separate tags for the I vs. J distinction and for the
individual opcodes between them.

[1]: https://github.com/zenhack/zero
[2]: https://doc.rust-lang.org/nomicon/data.html
