# Benchmarks

A deterministic benchmark harness for the purvasm oracle (ADR-0026). It sweeps a
suite of programs over input sizes and records two metrics per run — **steps**
(CESK machine transitions to a value) and **allocs** (store growth) — then plots
them with gnuplot.

These are reproducible *proxy* metrics for judging optimiser passes before the
bytecode VM exists; they are **not** wall-clock. Rationale and scope: ADR-0026
(and the round-trip methodology of ADR-0025).

## Running

From the `boot/` directory:

```sh
dune exec bench/bench.exe
```

Optional arguments — input fixtures dir, then output dir:

```sh
dune exec bench/bench.exe -- bench/fixtures bench/out
```

Requires `gnuplot` on `PATH` (provided by the nix dev shell). If it is missing
the `.dat` and `.gp` files are still written; run `gnuplot plot.gp` in the output
dir afterwards.

## Output

Everything lands in `bench/out/` (git-ignored):

- `<bench>.dat` — whitespace columns: `size`, then `steps allocs` per variant.
- `steps.png`, `allocs.png` — one panel per bench, one curve per variant, log y.
- `plot.gp` — the generated gnuplot script.

## Reading the numbers

`steps`/`allocs` are exact and deterministic, so a delta between variants is
meaningful and diffable across commits. They are a *stage proxy* for runtime cost
— they count work the program does on the oracle, which correlates with, but is
not, the eventual VM's cost (the VM, once it exists, is benchmarked on the same
suite for ground truth). The `x` axis is the entry's `Int` argument.

## Baseline (regression check)

`baseline/` holds the committed **no-opt** `.dat` snapshot (variants `direct` and
`anf`, no optimiser passes). It is the regression reference: after a change,
re-run the suite and diff `out/<bench>.dat` against `baseline/<bench>.dat` —
the `size`, `direct_*`, and `anf_*` columns must be unchanged. An optimiser pass
adds further columns whose curves should sit *below* `anf`; once a genuine
improvement (or an intended baseline change) lands, refresh the snapshot with
`cp bench/out/*.dat bench/baseline/`.

## The suite

Each program is self-contained: a shared `Bench.List` (list type plus
`map`/`filter`/`foldr`/…) and minimal `Data.*` operator imports, so its link
closure stays small and it returns an `Int` for a clean, value-checked run. The
swept size is the entry's `Int` argument: Fibonacci index (`fib`), list length
(`quicksort`, `map-fold`), board size (`n-queens`), or tree depth (`bintree-*`).

| bench       | entry            | exercises                          |
| ----------- | ---------------- | ---------------------------------- |
| fib         | `Fib.run`        | non-tail recursion; arith/Ord      |
| quicksort   | `Quicksort.run`  | `filter` HOF, append, Ord, ADT     |
| n-queens    | `NQueens.run`    | backtracking, `foldr`, lists       |
| bintree-dfs | `BinTree.runDfs` | recursion over an ADT              |
| bintree-bfs | `BinTree.runBfs` | list-as-queue (`O(n^2)`), append   |
| map-fold    | `MapFold.run`    | higher-order `map` + `foldr`       |

## Variants — adding an optimiser pass

`bench.ml` holds a `variants` list of `(name, term -> term)`. Today:

- `direct` — the linked program, unchanged.
- `anf` — the ADR-0025 round-trip `rev_transl (transl t)` (the optimiser
  baseline).

To measure a new pass, append one entry, e.g.

```ocaml
"dictelim", (fun t -> rev_transl (dict_elim (transl t)))
```

Every `.dat` gains two columns and every plot gains a curve automatically — that
is the whole point of the harness: write a pass, re-run, read the delta.

## Regenerating fixtures

`fixtures/` holds pre-compiled CoreFn, so running the suite needs no PureScript
toolchain. Regenerate it only when changing the programs under `src/`. With
`purs` and a PureScript package set (the `.purs` dependencies — `prelude` etc.):

```sh
purs compile --codegen corefn "boot/bench/src/**/*.purs" "<pkgs>/**/*.purs"
```

Then copy each entry module's *transitive import closure* of `corefn.json` into
`boot/bench/fixtures/<Module>/corefn.json`. The entry modules are `Fib`,
`Quicksort`, `NQueens`, `BinTree`, `MapFold`; the closure is the fixpoint over the
`imports` field of each module's `corefn.json`. A short walk does it:

```python
import json, os, shutil

out, dst = "output", "boot/bench/fixtures"


def imports(m):
    p = f"{out}/{m}/corefn.json"
    if not os.path.exists(p):
        return []
    js = json.load(open(p))["imports"]
    return [".".join(i["moduleName"]) for i in js]


seen = set()
stack = ["Fib", "Quicksort", "NQueens", "BinTree", "MapFold"]
while stack:
    m = stack.pop()
    if m in seen or not os.path.exists(f"{out}/{m}/corefn.json"):
        continue
    seen.add(m)
    stack += imports(m)

shutil.rmtree(dst, ignore_errors=True)
for m in sorted(seen):
    os.makedirs(f"{dst}/{m}", exist_ok=True)
    shutil.copy(f"{out}/{m}/corefn.json", f"{dst}/{m}/corefn.json")
```
