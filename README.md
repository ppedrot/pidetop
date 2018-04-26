PIDEtop
=======

This repository provides sources for a toplevel plugin to make Coq speak the
PIDE protocol. If you are looking for a release to go with PIDE, checkout the
PIDEtop branch (the default branch of this repository). 

The source code can also be downloaded as an archive from the [downloads page](https://bitbucket.org/coqpide/pidetop/downloads) (Under the 'tags' tab).

Installing
----------

If you have installed Coq using Opam, you can use Opam to install pidetop:

```shell
opam repo add pide http://bitbucket.org/coqpide/opam.git
opam install pidetop
```

Alternatively, building by hand can be done with [Dune](https://github.com/ocaml/dune/)

```shell
$ jbuilder build @install
```

If you have built Coq locally, set the `OCAMLPATH` variable to the proper location, as in:
```shell
$ export OCAMLPATH=/path/to/coq && jbuilder build @install
```

For Windows, we provide a [pre-compiled binary](https://bitbucket.org/coqpide/pidetop/downloads/pidetop-windows-0.3.0.zip), unpack the zip file in the toploop directory of the Coq installation directory (usually, C:\Coq\lib\toploop)

Compatibility
-------------

PIDEtop requires Coq 8.9

License
-------

This software is available under LGPL license version 2.1 or at your option the MIT/X license.
