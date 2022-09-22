tff(semantics, logic, (
    $$iol == [ $$operator == $$out3,
               $$constrained == $$credulous,
               $$preference == [norm1, norm2, norm3] ] )).

tff(norm1, axiom, {$$norm} @ (heatingOn, ~windowOpen) ).
tff(norm2, axiom, {$$norm} @ ($true, windowOpen) ).
tff(norm3, axiom, {$$norm} @ ($true, heatingOn) ).

