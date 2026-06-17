<!--
Title format: [Feature|Bugfix|Refactor|Chore|Docs|Test|Perf|CI] <ADR-XXXX|subsystem|docs>: <summary>
e.g. [Bugfix] ADR-0019: keep voided effects from being dropped
-->

## What & why

<!-- One or two sentences. Link the relevant ADR / issue. -->

## Checklist

CI passing is enforced by the required `ci-gate` status check, not by a box here.
The items below are the human-judgment gates:

- [ ] **Docs updated** — any behaviour/feature/representation change is reflected in `docs/` and relevant ADR(s).
- [ ] **Tests** — added/updated for this change; **a bug fix includes a regression guard in the routinely-run lane** (unit / e2e), not only a slow `test:bin` script.
- [ ] **Design changes have an ADR** — added or updated, with its `Status` set (ADR-first).
- [ ] **No perf regression** — for optimizer / runtime / lowering changes, benchmarks compared (`node bench/run.mjs <dir>`; do not overwrite `bench/snapshots/baseline.json`). N/A otherwise.
- [ ] **Runtime GC-type / ABI / canonicalization** — changes to the value-type substrate or the host/runtime ABI keep cross-module type canonicalization intact. N/A otherwise.

<!-- Tick N/A items off too, or delete the lines that genuinely don't apply. -->
