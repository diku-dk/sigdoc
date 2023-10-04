## sigdoc

_A signature documentation tool for Standard ML_

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

The result of running `sigdoc` on a series of (signature and
implementation) files, is a set of files written to the working
directory.

Signatures within files must conform to the following snippet
structure:

    (** Header

	General comment, which may be multi-line or empty...

	*)

    signature A = sig
      type a
      ...
      val b : a -> unit
    end

    (**

	[type a] is a type.

    [b a] returns unit.

    [Discussion]

	This last entry can be omitted. Also, above it is
	possible to include text-blocks using indentation and
	it is possible to refer to exception specifications,
	structure specifications, include specifications, and
	datatype specifications.
	*)

Structure declarations within files are documented if they take one of
the following forms:

    (** SigDoc *)
    structure A : B ...

	(** SigDoc *)
	structure A :> B ...

In both cases `B` must be an identified signature identifier. A
structure declaration that occur first in a file (perhaps after a
single comment) is also documented provided the ascribed signature
identifier identifies a signature in the provided files.

### Usage

    bash-3.2$ ./sigdoc
    Usage: sigdoc [-libpath p] [-about f] [-logo s]
                  [-pkg f] FILES

     -libpath p : specify the path to the js-library and
                  style files, relative to the working
                  directory.
	 -about f   : specify a file with HTML to embed in an
				  About tab.
	 -logo s    : specify html that presents a logo.
	 -pkg f     : specify path to smlpkg package file to
				  read package versions from.
	FILES include .sml and .sig files, which may contain
	signatures and structures.
	For further information, please consult the Sigdoc
	documentation at http://github.com/diku-dk/sigdoc
