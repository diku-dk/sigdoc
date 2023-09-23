## sigdoc

_A signature documentation tool for Standard ML_

----

Sigdoc is a command-line tool (named `sigdoc`) for generating HTML
documentation for Standard ML signatures. It takes as arguments a set
of signature files, containing single signatures, and a set of
implementation files. Sigdoc scans the signature files to obtain the
set of signature identifiers. Then, for each signature identifier, the
tool generates a documentation page with documentation for each
specified identifier. Also, the tool scans the implementation files
for structure identifiers, identifying implementations of the
signatures and generates a documentation page with an index over
structure identifiers (with links to the signature documentation).

The generated HTML documentation embeds an auto-completing search
field that depends on the jquery library placed in the `jslib/`
subdirectory. For the generated code to work, copy the content of the
`jslib/` directory into the target directory for the documentation.

Based on the file paths, the tool also tries to identify if the
signature or structure stem from an `smlpkg` package, and if so, a link to the package is embedded in the generated HTML documentation.

The result of running `sigdoc` on a series of signature and
implementation files, is a set of files written to the working
directory.

Signature files must conform to the following structure:

    (** General comment, which may be multi-line... *)

    signature A = sig
      type a
      ...
      val b : a -> unit
    end

    (**

	[a] is a type.

    [b a] returns unit.

	*)


## TODO

Generate also a grouped module overview:

    Standard Basis Library
    ----------------------
    General
    Option
    List
    ...

    Extended Basis Library
    ----------------------
    Pretty
    Pickle
    SetFun
    MapFun
    Hash
    HashTable
    Stack
    Queue
    Heap
    ListSort
    Random
    Md5
    DiGraph
    Uref
    Cstring
    ParseComb

    Tool Library
    ------------
    Draw2d
    Levenstein
    Html
