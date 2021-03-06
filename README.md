# Preconditions for Non-termination based on Horn clauses (NtHorn)

NtHorn is a transformation-guided tool for inferring sufficient  preconditions for non-termination of a program based on (Constrained) Horn clauses. It uses techniques such as abstract interpretation, partial evaluation, constraint specialisation and counterexample-guided program transformation and decomposition. 




## Language and interface 

NtHorn is written in Ciao Prolog and is interfaced with Parma polyhedra
library and Yices2 SMT solver for manipulating constraints.  It uses
several reusable components such as Convex polyhedra analyser,
Query-answer tranformer etc. It also includes a Java library for
manipulating finite tree automata.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.18 or newer  (installed
from git repository with `./ciao-boot.sh local-install`)

## Build and installation

You can automatically fetch, build, and install NtHorn using:

`ciao get github.com/bishoksan/NtHorn`

The following dependendencies (including third-party code) will be
installed automatically:

1. [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for
   [Parma Polyhedra Library](https://bugseng.com/products/ppl/)
   (`ciao get ciao_ppl`)
2. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
3. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)
4. [RAHFT] (https://github.com/bishoksan/RAHFT)   
   (`ciao get github.com/bishoksan/RAHFT`)

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** it is recommended to define your own
_workspace directory_ and clone this repository. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`.
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the source directory.

## Usage

**Usage**: `nthorn` \<*input file containing a set of Horn clauses*\> [Options]

**Input**: a set of Horn clauses together with `special clauses` for a distinguished predicate  `init(X)`. They
are written using Prolog notation:

e.g. a clause is written as: `h(X):- C, b1(X1),...,bn(Xn).` where `C` is a comma separated linear arithmetic constraints (`X>=10, Y=X+1`)

 The `special clauses` are of the form  `init(X) :- C, b1(X1),...,bn(Xn).` 

**Options**: `-pe` (for control flow refinement with partial evaluation), 
                     `-clssplit` (clause splitting using potential ranking function)
                     
**Output**: Sufficient preconditions for non-termination of program in terms of initial state variables.


## Generate a standalone binary distribution

```sh
mkdir dist; cd dist
ciaoc_sdyn ../src/nthorn
```


This creates a platform specific binary `nthorn` at `dist/`
directory, together with the collection of shared libraries for the
dependencies.

## Using script to run the benchmarks
In order to run all the benchmarks containing in the foloder <Benchmarks> and produce statistics, please run the command

`./nthorn_run_bench.sh  <Benchmarks>`

The results will be in `result_non-termination.txt`


## Contact

Send your queries to `bishoksan@gmail.com`.
