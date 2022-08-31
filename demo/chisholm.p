thf(semantics, logic, (
    $iol == [ $output == $out3,
              $constrained == $credulous,
              $constraints == [~helping] ] )).

thf(norm1, axiom, [$true, helping]).
thf(norm2, axiom, [helping, telling]).
thf(norm3, axiom, [~helping, ~telling]).

thf(input1, hypothesis, ~helping).

thf(c1, conjecture, ~telling).
thf(c2, conjecture, ~helping).
