thf(semantics, logic, (
    $iol := [ $output := $out4,
              $constrained := $credulous,
              $constraints := [a, b, a & b] ] )).

thf(norm1, axiom, [a,x]).
thf(norm2, axiom, [b,y]).
thf(norm3, axiom, [x | y,z]).

thf(input1, hypothesis, a | b).

thf(c1, conjecture, x).
thf(c2, conjecture, y).
thf(c3, conjecture, y | x).


