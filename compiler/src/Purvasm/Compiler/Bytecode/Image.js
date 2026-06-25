// The IEEE-754 bits of a double, read back as a *signed* 64-bit integer and rendered as a
// decimal string — matching OCaml's `Int64.to_string (Int64.bits_of_float f)`, so a
// `Number` literal serialises byte-identically to boot.
export const floatBitsDecimalImpl = (f) => {
  const buf = new ArrayBuffer(8);
  new Float64Array(buf)[0] = f;
  return new BigInt64Array(buf)[0].toString();
};
