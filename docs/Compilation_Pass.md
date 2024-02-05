_WIP_

- Corefn module is translated into the first intermediate representaion: ECF (_Extended CoreFn_).
- After some optimization applied, ECF module is translated into the second intermediate representation: ELC (_Entiched Lambda Calculus_)
- Suffering more aggresive optimization pass, the ELC is translated into the last representation: LIR (_Linearized IR_)
- LIR is very much like the text representation of compiled object file.  
  Overall pass is described as follows:

```
CoreFn
↓・Typeclass method monomorphization
↓・Constrained function monomorphization
↓・Record worker/wrapper transformation
↓・pattern matching compilation
↓・Simple constant propagation
ECF
↓・beta-reduction
↓・more aggresive constant propagation
↓・resolving known record field lookup
↓・primitive operation substitution
ELC
↓・closure conversion
↓・Effect Magic Do
↓・ST Magic Do
・Dead assignment elimination
LIR
↓・Codegen
pmi file, pmo file
```

## Typeclass monomorphization

## Constrained function monomorphization

## Record worker/wrapper transformation
