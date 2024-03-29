# rio: Reasoner for Input/Output Logics
Author: [Alexander Steen](https://www.alexandersteen.de/) 

You may cite the tool as: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4540272.svg)](https://doi.org/10.5281/zenodo.4540272)

Input/Output (I/O) logics have been devised by D. Makinson and L. van der Torre [1]
as a class of formal systems for norm-based deontic reasoning. Intuitively,
they formalize the detachment process of obligations (referred to as *the output*)
from a given set of conditional norms and a specific situation (called *the input*).
It differs from other deontic logics (e.g., based on modal logics or extensions of it)
in the sense that the norms themselves are not part of the object logic and hence do
not carry truth values. Constrained I/O logic [2]
extend plain I/O logics by restricting the output set to be consistent with respect to
a given set of formulas, addressing deontic paradoxes such as contrary-to-duty situations
and other dilemmas.

In the standard formulation, there are four different principal output operators
called out<sub>1</sub>, out<sub>2</sub>, out<sub>3</sub> and out<sub>4</sub>.
out<sub>1</sub> allows for basic detachment, out<sub>2</sub> adds reasoning-by-cases
principles, and out<sub>3</sub> and out<sub>4</sub> augment out<sub>1</sub> and out<sub>2</sub>,
respectively, with (cumulative) transitivity. For each operator there exists a so-called throughput
variant out<sub>i</sub><sup>+</sup>. Constrained I/O logic filter excess output that is
inconsistent wrt. a set of constraints. Preferred output further filters the set
of consistent outputs such that only preferred sets (wrt. to given preference relation)
are returned. For details, cf. we refer to [1].

## The Tool

`rio` provides a TPTP-aligned [3] automated reasoning system for unconstrained and
constrained I/O logics based on the operators out<sub>i</sub> and out<sub>i</sub><sup>+</sup>,
1 ≤ i ≤ 4. It is implemented as a Scala application and based on the
`scala-tptp-parser` [4].

In short, the system allows you to specify a set of conditional norms and a number
of inputs (each of which describing aspects of the current situational context),
and provides automated means for inferring whether given obligations (also encoded
as formulas) can be derived. `rio` can also be used to infer the set of detached
obligations instead of checking detachment of given ones. See [Usage](#usage) below
for details.

It is inspired by the decision procedures introduced in [5], though the actual
procedures differ.


## Installation

The simplest method of obtaining `rio` is to download the provided `.jar` file of
the most recent release from GitHub, see https://github.com/aureleeNet/rio/releases.
You can also download the sources and build `rio` yourself.

### Using a pre-built built

When using a pre-built `.jar` file, you are already done. `rio` is run by 
(assuming that the `.jar` file is called `rio.jar` and in your current working directory) ...

```
> java -jar rio
usage: rio [-o <operator>] [-p <parameter>] [-c <constraint>] <problem file>
[...]
```

### Building from sources

In order to build `rio` from sources, you need the following software/packages:
+ Make
+ sbt (scala build tool)
+ JDK (≥ 11)
+ g++
+ zlib
+ must (installation described below)

Unpack the source archive of `rio`.
Download `must` from https://github.com/jar-ben/mustool/archive/refs/heads/master.zip and extract
the contents of the archive into a folder called `mustool` (important!) as top-level directory within the `rio`
folder. Your `rio` folder should now look like this (only directories shown):

```
.
├── contrib
├── examples
├── mustool
├── project
└── src
```

Run `make`. If no error occurs, there should be a `bin`
directory containing the `rio.jar` file. Also, there is a convenience executable `rio`
that just wraps the jar file with shell run script.

## Usage

`rio` requires a Java runtime environment (JRE), version ≥ 11, to be run. Older java versions
might also work, though this has not been tested. Additionally, `rio` is currently only tested on
Linux as it uses a shared PicoSAT library only provided for linux-x86-64 architecture. This will
be addressed soon (hopefully!).

```
> java -jar rio
usage: rio [-o <operator>] [-p <parameter>] [-c <constraint>] <problem file>

 <problem file> can be either a file name or '-' (without parentheses) for stdin.

 Options:
  -o <operator>
     If <problem file> does not contain a logic specification statement, explicitly set
     the output operator to be assumed to <operator>.
     This option is ignored, if <problem file> contains a logic specification statement.
     Supported <operator>s are: out1, out2, out3, out4

  -p <parameter>
     Set additional parameters to the output operator.
     This option is ignored, if <problem file> contains a logic specification statement.
     Multiple parameters can be passed.
     Supported <parameter>s are: throughput, constrained-credulous, constrained-skeptical

  -c <constraint>
     Pass constraints to the reasoning system. <constraint> must be a valid THF formula
     (without annotations). Multiple constraints can be passed by passing multiple
     respective -c options.
     This option is ignored, if <problem file> contains a logic specification statement.
     Constraints are ignored if neither -p constrained-credulous nor -p constrained-skeptical
     was passed.

  --version
     Prints the version number of the executable and terminates.

  --help
     Prints this description and terminates.
```

## Input and output formats

The input files for `rio` are plain text files according to the TPTP TFF language (typed first-order logic), though
only the propositional fragment of TFF is currently supported. The semantics of formula roles
differs from the TPTP standard (see below), and the problem needs to contain a logic specification
of the form 

```
tff(<name>, logic, $$iol == [<parameter> == <value>, ...]).
```

Alternatively, semantics parameters can also be passed using command-line arguments.

`rio` gives SZS status results as follows:

+ SZS status Success if the problem does not contain conjectured outputs (formulas of role `conjecture`).
  In this case, a SZS result of type ListOfFormulae containing the finite base of the output set is given.
+ SZS status Theorem if the problem contains a conjectured output (a formula of role `conjecture`) and
  the conjectured output is indeed in the output set.
+ SZS status CounterSatisfiable if the problem contains a conjectured output (a formula of role `conjecture`) and
  the conjectured output is not in the output set.
+ If the problem contains multiple conjectures, one output (Theorem or CounterSatisfiable)
  is given for each conjecture.

### Semantics specification

A problem's semantics is specified using a dedicated `logic` formula, as
foreseen in the currently explored extension of the TPTP library, cf.
http://tptp.org/TPTP/Proposals/LogicSpecification.html for details.

| Parameter       | Value                                         | Description                                                                                                                                                                                                                                                                                                                                         |
|-----------------|-----------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `$$operator`    | One of `$$out1`, `$$out2`, `$$out3`, `$$out4` | Select the output operator (required parameter)                                                                                                                                                                                                                                                                                                     |
| `$$throughput`  | `$true` or `$false`                           | Enable/Disable throughout (default: `$false`)                                                                                                                                                                                                                                                                                                       |
| `$$constrained` | `$$credulous` or `$$skeptical`                | Enable constrained output operators with credulous or skeptical net output function, respectively (default: unset)                                                                                                                                                                                                                                  |
| `$$constraints` | `[<list of formulas>]`                        | Set the constraints for constrained output. Ignored of `$$constrained` is not set (default: unset).                                                                                                                                                                                                                                                 |
| `$$preference`  | `[<list of name tuples>]`                     | A comma-separated list of tuples of the form `[name1,name2,...]`, where each identifier refers to the name of a norm in the problem file. Earlier norms are preferred over later norms, norms in the same tuple are equally preferred. Tuple symbols may be dropped if they are singletons. Ignored of `$$constrained` is not set (default: unset). |

### Problem contents

| Formula role | Description                                                                                                                                                                                                                                                                                                                                                                                 |
|--------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `axiom`      | TPTP formulas of role `axiom` specify the conditional norms. Every `axiom` formula needs to be an expression of the form `{$$norm} @ (left,right)` where both `left` and `right` are formulas. `left` is the *body* of the norm (the condition) and `right` is the *head* of the norm (the obligation). The can be arbitrarily many formulas of role `axiom` in a problem file (zero, too). |
| `hypothesis` | TPTP formulas of role `hypothesis` denote the inputs and are regular formulas. There can be arbitrarily many formulas of role `hypothesis` in the problem file (zero is ok).                                                                                                                                                                                                                |
| `conjecture` | TPTP formulas of role `conjecture` denote conjectured outputs. For each `conjecture`,  `rio` will check whether the formula is indeed in the output set and return a corresponding SZS result. There can be arbitrarily many formulas of role `conjecture` in the problem file (zero is ok).                                                                                                |

## Examples

### Use case 1: Check whether given obligations can be detached

In order to check whether a number of obligations can actually be derived from the
given set of norms and the situational context (the input), the obligations in question
are specified as THF formulas of role "conjecture":

Let's say this content is stored in a file named `out1.p`:
```
tff(semantics, logic, (
    $$iol == [ $$operator == $$out1 ] )).

tff(norm1, axiom, {$$norm} @ ($true, helping) ).
tff(norm2, axiom, {$$norm} @ (helping, telling) ).
tff(norm3, axiom, {$$norm} @ (~helping, ~telling) ).

tff(input1, hypothesis, ~helping).

tff(c1, conjecture, ~telling).
tff(c2, conjecture, ~helping).
```

Then you run `rio` via:
```
./rio out1.p
```
... producing the following output:
```
% SZS status Theorem for src/test/resources/out1.p: c1
% SZS status CounterSatisfiable for src/test/resources/out1.p: c2
```

As can be seen, only one of the conjectures (only `c1`) is indeed detached.
As a consequence, SZS status Theorem is given for `c1` but SZS status
CounterSatisfiable is given for conjecture `c2`.

### Use case 2: Generate set of outputs

If no conjectures are contained in the problem file, e.g., as in ...

```
thf(semantics, logic, (
    $$iol == [ $$operator == $$out3 ] )).

thf(norm1, axiom, {$$norm} @ ($true, helping) ).
thf(norm2, axiom, {$$norm} @ (helping, telling) ).
thf(norm3, axiom, {$$norm} @ (~helping, ~telling) ).

thf(input1, hypothesis, ~helping).
```

`rio` will instead output a complete finite basis of detachable formulas and SZS status Success:

```
% SZS status Success for out3.p
% SZS output start ListOfFormulae for out3.p
$false
% SZS output end ListOfFormulae for out3.p
```

A finite basis is a finite set of formulas A such that every detachable output x
follows from A, i.e., A ⊧ x. This allows to have a finite representation for
the infinite set of outputs. In the above case, every output x is in the output
set as {⊥} ⊧ x for every x.


### Use case 3: Constrained outputs

In the previous example (use case 2), the output set becomes too large as the
input and the set of norm entail an inconsistency, known as Contrary To Duty
situations in the Deontic logic community.

In the context of I/O logics, the output set can be constrained to remedy this.
This is reflected in `rio` by enabling constrained I/O operations via the logic
specification:

```
thf(semantics, logic, (
    $$iol == [ $$operator == $$out3,
               $$constrained == $$credulous,
               $$constraints == [~helping] ] )).

thf(norm1, axiom, {$$norm} @ ($true, helping) ).
thf(norm2, axiom, {$$norm} @ (helping, telling) ).
thf(norm3, axiom, {$$norm} @ (~helping, ~telling) ).

thf(input1, hypothesis, ~helping).
```


In this case, the output is constrained to be the credulous output aggregate of 
all maximally consistent norm families wrt. detachment via out<sub>3</sub>
and the constraint `~helping`. See [2] for the theoretical details.

This problem file gives a consistent output set as follows:


```
% SZS status Success for out3-constrained.p
% SZS output start ListOfFormulae for out3-constrained.p
~ telling
% SZS output end ListOfFormulae for out3-constrained.p
```

### Use case 4: Preferred output

In the constrained case, a preference ordering can optionally be imposed to
further restrict the generated outputs. 

```
tff(semantics, logic, (
    $$iol == [ $$operator == $$out3,
               $$constrained == $$credulous,
               $$preference == [norm1, norm2, norm3] ] )).

tff(norm1, axiom, {$$norm} @ (heatingOn, ~windowOpen) ).
tff(norm2, axiom, {$$norm} @ ($true, windowOpen) ).
tff(norm3, axiom, {$$norm} @ ($true, heatingOn) ).
```


In this case, the output is constrained to be the credulous output aggregate of
all maximally consistent norm families wrt. detachment via out<sub>3</sub>
(and the empty set of constraints). Additionally, the preference ordering
`name1 > name2 > name3` is declared. See [6] for details.

This problem file gives a consistent output set as follows:


```
% SZS status Success for examples/preferred.p
% SZS output start ListOfFormulae for examples/preferred.p
windowOpen
% SZS output end ListOfFormulae for examples/preferred.p
```
 
## Usage as library/API

`rio`'s reasoning procedure can also be invoked by adding the .jar file to
your project's classpath and calling the `Reasoner.apply` method directly.
 
## License

`rio` is distributed under the BSD 3-Clause license (see LICENSE file),
and uses third party libraries that are distributed under their own terms
(see LICENSE-3RD-PARTIES file).

In particular, `rio` uses

- picosat, Copyright (c) 2006 - 2014, Armin Biere, Johannes Kepler University (http://fmv.jku.at/picosat/).
- MUST (mustool), Copyright (c) 2019, Jaroslav Bendik, Masaryk University (https://github.com/jar-ben/mustool).
- MiniSat (contained in MUST), Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson and Copyright (c) 2007-2010  Niklas Sorensson (http://minisat.se/).
  


## References

[1] Makinson, D., van der Torre, L.W.N.: Input/Output Logics. J. Philosophical Logic 29(4), 383–408 (2000). https://doi.org/10.1023/A:1004748624537

[2] Makinson, D., van der Torre, L.W.N.: Constraints for Input/Output Logics. J. Philos. Log. 30(2): 155-185 (2001). https://doi.org/10.1023/A:1017599526096

[3] Sutcliffe, G, and Suttner, C.: The TPTP Problem Library for Automated Theorem Proving. http://tptp.org.

[4] Steen, A.: `scala-tptp-parser` (Version v1.2). Zenodo, 2021. DOI: http://doi.org/10.5281/zenodo.4468959

[5] Steen, A.: Goal-Directed Decision Procedures for Input/Output Logics. In 15th International Conference on Deontic Logic and Normative Systems (DEON 2020/2021, Munich), Fenrong Liu, Alessandra Marra, Paul Portner, and Frederik Van De Putte (Eds.), College Publications, London, 2021. (to appear). See: http://www.collegepublications.co.uk/DEON/?00003

[6] Parent, X.: Moral particularism in the light of deontic logic. Artif. Intell. Law 19(2-3): 75-98 (2011)

