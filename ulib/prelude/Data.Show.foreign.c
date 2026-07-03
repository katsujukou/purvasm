/*
 * Data.Show.foreign.c — the prelude ulib's native `foreign` leaves (ADR-0073).
 *
 * `Data.Show` reimplements most of its foreigns in PureScript over `Purvasm.*` (ADR-0038), but
 * `showNumberImpl` is a residual first-order function that genuinely needs native code: the shortest
 * round-tripping decimal of an IEEE-754 double is not expressible over the primitives. So the ulib ships
 * it here as C over the `pv_*` C-ABI (ADR-0073 §2); the build compiles it and the linker resolves the
 * `pvf_Data_2eShow_2eshowNumberImpl` symbol (ADR-0073 §3).
 *
 * The formatting is byte-identical to boot's OCaml oracle (`Ffi.show_number`): both defer to the platform
 * libc for `%.0f` / `%g`, so the LLVM-backend output matches the CESK oracle in the differential.
 */
#include "purvasm.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * `showNumberImpl :: Number -> String` — the prelude's `n.toString()` with the `"x.0"` rule for integral
 * values. Reads the `Number`'s IEEE-754 value via `pv_number_bits` (representation stays opaque) and
 * builds one `String` via `pv_new_str`. A single allocation, so no rooting is needed (see purvasm.h).
 */
PVWord pvf_Data_2eShow_2eshowNumberImpl(PVContext *ctx, PVWord closure,
                                        const PVWord *args, size_t nargs) {
    (void)closure;
    (void)nargs;

    uint64_t bits = pv_number_bits(ctx, args[0]);
    double f;
    memcpy(&f, &bits, sizeof f); /* the bit pattern → the double it denotes */

    char buf[32];
    int len;

    if (isnan(f)) {
        len = snprintf(buf, sizeof buf, "NaN");
    } else if (f == (double)INFINITY) {
        len = snprintf(buf, sizeof buf, "Infinity");
    } else if (f == (double)(-INFINITY)) {
        len = snprintf(buf, sizeof buf, "-Infinity");
    } else if (f == 0.0) {
        /* also catches -0.0 (== 0.0 in C), which JS prints as "0" → "0.0" */
        len = snprintf(buf, sizeof buf, "0.0");
    } else if (f == floor(f) && fabs(f) < 1e21) {
        /* integral in fixed-notation range: print without an exponent, add the ".0" */
        len = snprintf(buf, sizeof buf, "%.0f.0", f);
    } else {
        /* fractional (or beyond fixed-notation range): the shortest `%g` that round-trips. `%g` only
           resorts to an exponent for genuinely extreme values, matching JS there; a fractional value
           never needs the ".0" suffix. */
        int p = 1;
        for (; p <= 17; p++) {
            snprintf(buf, sizeof buf, "%.*g", p, f);
            if (strtod(buf, NULL) == f) {
                break;
            }
        }
        if (p > 17) {
            snprintf(buf, sizeof buf, "%.17g", f);
        }
        len = (int)strlen(buf);
    }

    return pv_new_str(ctx, (const uint8_t *)buf, (size_t)len);
}
