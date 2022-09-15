tff(semantics, logic, (
    $$iol == [ $$operator == $$out3 ] )).

tff(norm1, axiom, {$$norm} @ ($true, helping) ).
tff(norm2, axiom, {$$norm} @ (helping, telling) ).
tff(norm3, axiom, {$$norm} @ (~helping, ~telling) ).

tff(input1, hypothesis, ~helping).

%-Uncomment if specific goals should be checked
%tff(c1, conjecture, ~telling).
%tff(c2, conjecture, ~helping).
