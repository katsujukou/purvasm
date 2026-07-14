// E2E check for `ulib/strings`' Data.String.Internal.CaseMap / Data.String.Common (case mapping).
//
// Unlike `ulib-tools unicode-gen`'s own self-check (Purvasm.UlibTools.UnicodeData.selfCheck,
// ADR-0101 -- which only inspects the parsed (cp, mapped) pairs before rendering), this exercises
// the ACTUAL COMPILED PureScript: the rendered table literals,
// `CaseMap.lookupCp`'s binary search, and `Data.String.Common.mapCase`'s two-pass UTF-8
// byte-length/`putCp` reconstruction -- so a binary-search off-by-one, an upper/lower table swap,
// a `renderTable` corruption, or a broken byte-length-changing rebuild would fail HERE, not just
// in `ulib-tools unicode-gen`.
//
// Usage: node verify-case-mapping.mjs <compiled-output-dir>
// (run via verify-case-mapping.sh, which stages+compiles the real ulib+registry tree first).

const outDir = process.argv[2];
if (!outDir) {
  console.error("usage: node verify-case-mapping.mjs <purs --output dir>");
  process.exit(2);
}

const CaseMap = await import(`${outDir}/Data.String.Internal.CaseMap/index.js`);
const Common = await import(`${outDir}/Data.String.Common/index.js`);

let failures = 0;

function checkEq(label, actual, expected) {
  if (actual !== expected) {
    console.error(`FAIL ${label}: got ${actual} (0x${(actual >>> 0).toString(16)}), expected ${expected} (0x${(expected >>> 0).toString(16)})`);
    failures++;
  } else {
    console.log(`ok   ${label}`);
  }
}

// `Purvasm.String`'s JS shim models "byte" as a UTF-16 code unit (correct for JS-hosted code) --
// to exercise `mapCase`'s UTF-8 byte-length arithmetic the way the real native backend actually
// sees it, encode to raw UTF-8 bytes and pack each byte into one JS char code (a "byte string"),
// matching a native String's byte array. See Data.String.Common's own doc comment.
function toByteString(s) {
  return Buffer.from(s, "utf8").toString("latin1");
}
function fromByteString(bs) {
  return Buffer.from(bs, "latin1").toString("utf8");
}

function checkStr(label, actualBytes, expected) {
  const actual = fromByteString(actualBytes);
  if (actual !== expected) {
    console.error(`FAIL ${label}: got ${JSON.stringify(actual)}, expected ${JSON.stringify(expected)}`);
    failures++;
  } else {
    console.log(`ok   ${label}`);
  }
}

// -- Data.String.Internal.CaseMap: the generated table + lookupCp binary search, on raw code
// -- points (representation-independent -- these numbers mean the same thing on every backend).

// Table head (the lowest code point either table maps): 'a'/'A', U+0061/U+0041.
checkEq("toUpperCp head 'a'", CaseMap.toUpperCp(0x61), 0x41);
checkEq("toLowerCp head 'A'", CaseMap.toLowerCp(0x41), 0x61);

// Table tail (the highest code point either table maps, Unicode 15.0.0): Adlam small/capital SHA,
// U+1E943/U+1E921 -- also the astral (4-byte-UTF-8) end of the range.
checkEq("toUpperCp tail (Adlam SHA)", CaseMap.toUpperCp(0x1e943), 0x1e921);
checkEq("toLowerCp tail (Adlam SHA)", CaseMap.toLowerCp(0x1e921), 0x1e943);

// The regression case this whole file exists for: U+0130 (İ) has a real SIMPLE lowercase (U+0069),
// distinct from a host case-folding routine's FULL lowercase (U+0069 U+0307, two code points).
checkEq("toLowerCp U+0130 (İ)", CaseMap.toLowerCp(0x0130), 0x0069);
checkEq("toUpperCp U+0131 (ı)", CaseMap.toUpperCp(0x0131), 0x0049);

// A documented 1:N exclusion: U+00DF (ß) has NO simple uppercase (its only uppercase form, "SS", is
// SpecialCasing.txt-only) -- must fall through to the identity, not some wrong mapped value.
checkEq("toUpperCp U+00DF (ß) -> identity", CaseMap.toUpperCp(0x00df), 0x00df);
checkEq("toLowerCp U+1E9E (ẞ)", CaseMap.toLowerCp(0x1e9e), 0x00df);

// Unregistered / boundary code points must be the identity (absent from the table).
checkEq("toUpperCp U+0000 (NUL) -> identity", CaseMap.toUpperCp(0x0000), 0x0000);
checkEq("toLowerCp U+4E2D (CJK 中) -> identity", CaseMap.toLowerCp(0x4e2d), 0x4e2d);
checkEq("toUpperCp U+10FFFF (max scalar) -> identity", CaseMap.toUpperCp(0x10ffff), 0x10ffff);

// -- Data.String.Common: mapCase's two-pass UTF-8 byte-length computation + `putCp` rebuild, over
// -- genuine UTF-8 byte strings (matching what the native backend's `Purvasm.String` really is).

checkStr("toUpper ASCII", Common.toUpper(toByteString("hello")), "HELLO");
checkStr("toLower ASCII", Common.toLower(toByteString("HELLO")), "hello");

// U+017F 'ſ' (2 UTF-8 bytes) -> U+0053 'S' (1 byte): the byte-length-SHRINKING case mapCase's
// two-pass size computation must get right (a naive same-length rebuild would corrupt this).
checkStr("toUpper U+017F 'ſ' -> 'S' (2 bytes -> 1)", Common.toUpper(toByteString("ſ")), "S");

checkStr("toLower U+0130 'İ' -> 'i'", Common.toLower(toByteString("İ")), "i");
checkStr("toUpper U+0131 'ı' -> 'I'", Common.toUpper(toByteString("ı")), "I");
checkStr("toUpper U+00DF 'ß' unchanged", Common.toUpper(toByteString("ß")), "ß");
checkStr("toLower U+1E9E 'ẞ' -> 'ß'", Common.toLower(toByteString("ẞ")), "ß");

// The astral (4-byte UTF-8) table tail, round-tripped through the real string-level API too.
checkStr(
  "toUpper Adlam small SHA (astral, 4-byte UTF-8)",
  Common.toUpper(toByteString("\u{1E943}")),
  "\u{1E921}",
);

// A mixed ASCII + same-length non-ASCII string (the two-pass logic must handle both in one pass).
checkStr("toUpper mixed café", Common.toUpper(toByteString("café")), "CAFÉ");

if (failures > 0) {
  console.error(`\n${failures} check(s) FAILED`);
  process.exit(1);
}
console.log("\nall checks passed");
