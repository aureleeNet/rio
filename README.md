# Rio: Reasoner for Input/Output Logics

Input/Output (I/O) logics have been devised by Makinson and van der Torre [1]
as a class of formal systems for norm-based deontic reasoning. Intuitively,
they formalize the question what obligations can be detached (the output) from a given
set of conditional norms and a specific situation (the input). It differs from
other deontic logics in the sense that the norms themselves are not part of the
object logic and hence do not carry truth values. Constrained I/O logic [2]
addresses contrary-to-duty situations and other dilemmas.


## The Tool

Rio provides a TPTP-compatible automated reasoning system for various I/O logics.
In short, the system allows you to input a set of norms and an input
(the description of the current situation), and provides automated means for
inferring whether a certain formula can be derived as an obligation from this.
See [Usage](#usage) below for details.

It is implemented as a Scala application and library. It is inspired by the
decision procedures introduced in [3], though the actual decision procedures differ a bit.


## Installation

... todo ...


## Usage

... todo ...
 
## License

Rio is licensed using the BSD 3-Clause license (see LICENSE file),
and uses third party libraries that are distributed under their own terms (see LICENSE-3RD-PARTIES file).

## References

[1] Makinson, D., van der Torre, L.W.N.: Input/Output Logics. J. Philosophical Logic 29(4), 383–408 (2000). https://doi.org/10.1023/A:1004748624537

[2] Makinson, D., van der Torre, L.W.N.: Constraints for Input/Output Logics. J. Philos. Log. 30(2): 155-185 (2001). https://doi.org/10.1023/A:1017599526096

[3] Alexander Steen, Goal-Directed Decision Procedures for Input/Output Logics. In 15th International Conference on Deontic Logic and Normative Systems (DEON 2020/2021, Munich), Fenrong Liu, Alessandra Marra, Paul Portner, and Frederik Van De Putte (Eds.), College Publications, London, 2021. (to appear). See: http://www.collegepublications.co.uk/DEON/?00003
