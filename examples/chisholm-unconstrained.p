thf(semantics, logic, (
    $iol == [ $output == $out3 ] )).

thf(norm1, axiom, [$true, helping]).
thf(norm2, axiom, [helping, telling]).
thf(norm3, axiom, [~helping, ~telling]).

thf(input1, hypothesis, ~helping).

%-Uncomment if specific goals should be checked
%thf(c1, conjecture, ~telling).
%thf(c2, conjecture, ~helping).
