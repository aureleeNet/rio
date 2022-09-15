tff(semantics, logic, (
    $$iol == [ $$operator == $$out1 ] )).

tff(norm1, axiom, {$$norm} @ ($true, helping) ).
tff(norm2, axiom, {$$norm} @ (helping, telling) ).
tff(norm3, axiom, {$$norm} @ (~helping, ~telling) ).

tff(input1, hypothesis, ~helping).

tff(c1, conjecture, ~telling).
tff(c2, conjecture, ~helping).
