//! `PURVASM_HEAP_WORDS` parsing (ADR-0102 §4): lets heap-size experiments run without relinking a
//! native compiler — an explicit env-var override of the codegen-provided default semi-space word
//! count, scoped to measurement and OOM/GC-cliff diagnosis only. Not heap growth and not a GC design
//! change: the override still produces a single fixed-size semi-space pair, just a differently-sized
//! one.
//!
//! This module is pure (no `Heap`/environment access) so [`parse_heap_words_env`] is unit-tested
//! directly; [`crate::gc::Heap::new_native`] owns the actual env-var read.

/// Parse the `PURVASM_HEAP_WORDS` environment variable's value (ADR-0102 §4): absent means "use the
/// codegen-provided default" (`Ok(None)`); a present value must be a non-empty positive decimal
/// `usize` (`Ok(Some(n))`). Anything else — empty, zero, overflow, a leading sign character, a
/// separator, whitespace, or any non-decimal byte — is an error carrying a diagnostic message. The
/// caller ([`Heap::new_native`](crate::gc::Heap::new_native)) turns that into a panic (which the
/// `pv_*` ABI's `guard` escalates to `process::abort()`). Pure: takes the already-decoded value so it
/// is unit-testable without touching the real environment.
pub(crate) fn parse_heap_words_env(raw: Option<&str>) -> Result<Option<usize>, String> {
    let s = match raw {
        None => return Ok(None),
        Some(s) => s,
    };
    // A non-empty, all-ASCII-digit check up front rejects empty, leading `+`/`-`, separators
    // (`_`/`,`), whitespace, and any non-decimal byte in one place — `usize::parse` alone would
    // silently *accept* a leading `+` for an unsigned type, which the ADR explicitly rejects.
    if s.is_empty() || !s.bytes().all(|b| b.is_ascii_digit()) {
        return Err(format!(
            "PURVASM_HEAP_WORDS: expected a non-empty positive decimal integer, got {s:?}"
        ));
    }
    match s.parse::<usize>() {
        Ok(0) => Err(format!(
            "PURVASM_HEAP_WORDS: expected a positive decimal integer, got {s:?}"
        )),
        Ok(n) => Ok(Some(n)),
        Err(_) => Err(format!("PURVASM_HEAP_WORDS: value {s:?} overflows usize")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absent_means_use_the_codegen_default() {
        assert_eq!(parse_heap_words_env(None), Ok(None));
    }

    #[test]
    fn valid_positive_decimal_is_accepted() {
        assert_eq!(parse_heap_words_env(Some("1")), Ok(Some(1)));
        assert_eq!(parse_heap_words_env(Some("8388608")), Ok(Some(8_388_608)));
        // Leading zeros are not in the ADR's rejection list — the decimal *value* is what matters.
        assert_eq!(parse_heap_words_env(Some("007")), Ok(Some(7)));
        assert_eq!(
            parse_heap_words_env(Some(&usize::MAX.to_string())),
            Ok(Some(usize::MAX)),
            "usize::MAX itself is valid, not an overflow"
        );
    }

    #[test]
    fn empty_is_rejected() {
        assert!(parse_heap_words_env(Some("")).is_err());
    }

    #[test]
    fn zero_is_rejected() {
        for bad in ["0", "00", "0000"] {
            assert!(
                parse_heap_words_env(Some(bad)).is_err(),
                "{bad:?} must be rejected"
            );
        }
    }

    #[test]
    fn overflow_is_rejected() {
        // One more digit than `usize::MAX` guarantees overflow regardless of target width.
        let too_big = format!("9{}", usize::MAX);
        let err = parse_heap_words_env(Some(&too_big)).expect_err("must overflow usize");
        assert!(
            err.contains("overflows"),
            "diagnostic should say overflow: {err}"
        );
    }

    #[test]
    fn leading_sign_characters_are_rejected() {
        for bad in ["+123", "-123", "+0", "-1"] {
            assert!(
                parse_heap_words_env(Some(bad)).is_err(),
                "{bad:?} must be rejected"
            );
        }
    }

    #[test]
    fn separators_are_rejected() {
        for bad in ["1_000", "1,000", "1.000"] {
            assert!(
                parse_heap_words_env(Some(bad)).is_err(),
                "{bad:?} must be rejected"
            );
        }
    }

    #[test]
    fn whitespace_is_rejected() {
        for bad in [" 123", "123 ", "1 000", "\t123", "123\n"] {
            assert!(
                parse_heap_words_env(Some(bad)).is_err(),
                "{bad:?} must be rejected"
            );
        }
    }

    #[test]
    fn non_decimal_bytes_are_rejected() {
        for bad in ["abc", "12a", "0x10", "１２３"] {
            assert!(
                parse_heap_words_env(Some(bad)).is_err(),
                "{bad:?} must be rejected"
            );
        }
    }

    #[test]
    fn every_rejection_names_the_variable() {
        for bad in ["", "0", "+1", "1_000", " 1", "abc"] {
            let err = parse_heap_words_env(Some(bad)).expect_err("must be rejected");
            assert!(
                err.contains("PURVASM_HEAP_WORDS"),
                "diagnostic should name the variable: {err}"
            );
        }
    }
}
