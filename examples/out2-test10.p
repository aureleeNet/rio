tff(semantics, logic, (
    $$iol == [ $$operator == $$out2 ] )).

tff(norm1, axiom, {$$norm} @ (a, x) ).
tff(norm2, axiom, {$$norm} @ (~a, y) ).
tff(norm3, axiom, {$$norm} @ (b, z) ).

tff(input1, hypothesis, a & ~a).
