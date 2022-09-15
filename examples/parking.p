tff(semantics, logic, (
    $$iol == [ $$operator == $$out4 ] )).

tff(norm1, axiom, {$$norm} @ (parking, ticket | fine) ).
tff(norm2, axiom, {$$norm} @ (ticket, pay) ).
tff(norm3, axiom, {$$norm} @ (fine, pay) ).

tff(input1, hypothesis, parking).
