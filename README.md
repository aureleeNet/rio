# rio: Reasoner for Input/Output Logics

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

In the standard formulation, there are four different (unconstrained) output operators
called out<sub>1</sub>, out<sub>2</sub>, out<sub>3</sub> and out<sub>4</sub>.
out<sub>1</sub> allows for basic detachment, out<sub>2</sub> adds reasoning-by-cases
principles, and out<sub>3</sub> and out<sub>4</sub> augment out<sub>1</sub> and out<sub>2</sub>,
respectively, with (cumulative) transitivity. For details, cf. we refer to [1].

## The Tool

`rio` provides a TPTP-aligned [3] automated reasoning system for unconstrained and
constrained I/O logics basid on the out<sub>i</sub> operators, 1 ≤ i ≤ 4.
It is implemented as a Scala application and based on the
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

When using a pre-built `.jar` file, you are already done. You can run `rio` by 
(assuming that the `.jar` file is called `rio.jar` and in your current working directory) ...

```
> java -jar rio
usage: rio [-o <operator>] [-p <parameter>] [-c <constraint>] <problem file>
[...]
```

### Building from sources

In order to build `rio` from sources, you need the following software:
+ Make
+ sbt (scala build tool)
+ JDK (≥ 11)

Unpack the source archive, and run `make`. If no error occurs, there should be a `bin`
directory containing the `rio.jar` file. Also, there is a convenience executable `rio`
that just wraps the jar file with shell run script.

## Usage

`rio` requires a Java runtime environment (JRE), version ≥ 11, to be run. Older java versions
might also work, though this has not been tested.

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

...

## Examples

### Use case 1: Check whether given obligations can be detached

In order to check whether a number of obligations can actually be derived from the
given set of norms and the situational context (the input), the obligations in question
are specified as THF formulas of role "conjecture":

Let's say this content is stored in a file named `out1.p`:
```
thf(semantics, logic, (
    $iol := [ $output := $out1 ] )).

thf(norm1, axiom, [$true, helping]).
thf(norm2, axiom, [helping, telling]).
thf(norm3, axiom, [~helping, ~telling]).

thf(input1, hypothesis, ~helping).

thf(c1, conjecture, ~telling).
thf(c2, conjecture, ~helping).
```

Then you run `rio` via:
```
./rio out1.p
```
... producing the following output:
```
% SZS status WeakerConclusion for out1.p: Only some of the conjectured outputs are indeed in the out set.
% SZS output start ListOfFormulae for out1.p
~ telling
% SZS output end ListOfFormulae for out1.p
```

As can be seen, only one of the conjectures (only `c1`) is indeed detached.

### Use case 2: Generate set of obligations

...
 
## License

`rio` is distributed under the BSD 3-Clause license (see LICENSE file),
and uses third party libraries that are distributed under their own terms
(see LICENSE-3RD-PARTIES file).

## References

[1] Makinson, D., van der Torre, L.W.N.: Input/Output Logics. J. Philosophical Logic 29(4), 383–408 (2000). https://doi.org/10.1023/A:1004748624537

[2] Makinson, D., van der Torre, L.W.N.: Constraints for Input/Output Logics. J. Philos. Log. 30(2): 155-185 (2001). https://doi.org/10.1023/A:1017599526096

[3] Sutcliffe, G, and Suttner, C.: The TPTP Problem Library for Automated Theorem Proving. http://tptp.org.

[4] Steen, A.: `scala-tptp-parser` (Version v1.2). Zenodo, 2021. DOI: http://doi.org/10.5281/zenodo.4468959

[5] Steen, A.: Goal-Directed Decision Procedures for Input/Output Logics. In 15th International Conference on Deontic Logic and Normative Systems (DEON 2020/2021, Munich), Fenrong Liu, Alessandra Marra, Paul Portner, and Frederik Van De Putte (Eds.), College Publications, London, 2021. (to appear). See: http://www.collegepublications.co.uk/DEON/?00003
