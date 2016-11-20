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
  sharing variables. `flags_and_limit_high` is conceptually a four-bit
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
with good reasons. [See this section of the Rustonomicon][2] for details

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

## Some rough ideas

I'd like to be able to define two things:

1. The logical structure of the data type
2. Its physical layout.

Example:

    type GDTEnt struct {
        base: uint32
        limit: uint20

        flags: struct {
            gr, sz: bool
        }

        access: struct {
            ac, rw, dc, ex, pr: bool
            privl: uint2
        }
    }

    layout GDTEnt {
        limit[0:16]
        base[0:24]
        access {
            ac, rw, dc, ex, 1: bits(1)
            privl: bits(2)
            pr: bits(1)
        }
        limit[16:20]
        flags {
            0: uint2
            sz: bit
            gr: bit
        }
        base[24:32]
    }

A tool could then be used to generate C code that could be called like
so:

    set_base(&ent, 0xffffffff);
    uint32_t lim = get_limit(&ent);

Or in a language that has more a bit more powerful mechanisms for
abstraction, such as C++ or rust:

    ent.base = 0xffffffff;
    uint32_t lim = ent.limit;

## Questions:

### Sum types?
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

    type T union {
        foo struct {
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

### Pointers/References?

* Relative vs absolute?
* Physical/virtual address distinction?
* "Far" pointers? Places this shows up:
  * real mode x86
  * Cap'N Proto

I see two ways of approaching this:

* Mostly punt; keep this somewhat impoverished
* Try to come up with a way to compose things. We're not going to
  capture every possible addressing model by just enumerating them.

### Variable length fields?

### Scope?

I'm pretty sure I want at least the sum-type functionality, but the
pointer stuff gets a little trickier, and you could go pretty deep with
this. Need to better define the scope of things we want to deal with.

### What code to gen?

* What should the exposed APIs look like? What tools would be useful?

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

### Mips encoding

Mips has three basic encoding types: R, I, and J. In all cases, the
first six bits are an opcode. For R-Type instructions, the opcode field
is zero, and there is a secondary 'funct' field elsewhere in the
instruction. The I and J types only have one opcode.

The naive sum type solution above doesn't work here, since you don't
have two separate tags for the I vs. J distinction and for the
individual opcodes between them.
