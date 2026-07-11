/*
 * ulib native foreign for `numbers`' `Data.Number` math family (ADR-0042 / ADR-0073), targeting
 * numbers 9.0.1: pure libm leaves over the `pv_*` C-ABI, JS-faithful where libm and ECMAScript
 * disagree (`round` is half-toward-+inf with the -0.0 cases, not C `round`'s half-away-from-zero).
 * Held at parity with boot's `Ffi.host` arms by the differential (ADR-0072 §10).
 *
 * Every leaf is a single-allocation pure function (one `pv_new_number`/`pv_bool` after all reads),
 * so none needs the rooting ceremony (ADR-0073 §2).
 */
#include "purvasm.h"

#include <math.h>
#include <string.h>

static double num_arg(PVContext *ctx, PVWord w) {
  uint64_t bits = pv_number_bits(ctx, w);
  double d;
  memcpy(&d, &bits, sizeof d);
  return d;
}

static PVWord mk_number(PVContext *ctx, double d) {
  uint64_t bits;
  memcpy(&bits, &d, sizeof bits);
  return pv_new_number(ctx, bits);
}

#define UNARY_NUM(sym, expr)                                                            \
  PVWord sym(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {            \
    (void)clo;                                                                          \
    (void)nargs;                                                                        \
    double x = num_arg(ctx, args[0]);                                                   \
    return mk_number(ctx, (expr));                                                      \
  }

UNARY_NUM(pvf_Data_2eNumber_2eabs, fabs(x))
UNARY_NUM(pvf_Data_2eNumber_2efloor, floor(x))
UNARY_NUM(pvf_Data_2eNumber_2eceil, ceil(x))
UNARY_NUM(pvf_Data_2eNumber_2esin, sin(x))

/* ECMAScript `Math.round` (ADR-0042): half-toward-+inf — `floor(x + 0.5)` — with NaN/±inf passed
 * through and the `-0.0` band (`-0.5 <= x < 0`) yielding `-0.0`; NOT C `round` (half-away-from-zero).
 * Byte-parity with boot's `js_round`. */
PVWord pvf_Data_2eNumber_2eround(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  double x = num_arg(ctx, args[0]);
  double r;
  if (!isfinite(x)) {
    r = x;
  } else {
    r = floor(x + 0.5);
    if (r == 0.0 && signbit(x)) r = -0.0;
  }
  return mk_number(ctx, r);
}

PVWord pvf_Data_2eNumber_2eisFinite(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_bool(isfinite(num_arg(ctx, args[0])));
}

PVWord pvf_Data_2eNumber_2eisNaN(PVContext *ctx, PVWord clo, const PVWord *args, size_t nargs) {
  (void)clo;
  (void)nargs;
  return pv_bool(isnan(num_arg(ctx, args[0])));
}
