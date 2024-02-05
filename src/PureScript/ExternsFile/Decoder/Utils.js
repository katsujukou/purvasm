import { Left, Right } from "../Data.Either/index.js";
import { Unexpected, MissingValue } from "../PureScript.ExternsFile.Decoder.Monad/index.js";

export const readAt = function (idx, fgn) {
    if (!Array.isArray(fgn)) {
      return Left.create(Unexpected.create("Expecting array, got " + typeof fgn))
    }
    if (fgn[idx] === void 0) {
      return Left.create (MissingValue.value);
    }
    if (fgn.length < idx) {
      return Left.create(Unexpected.create("Got an array of length " + fgn.length + ", which is too small to get element at " + idx));
    }
    return Right.create(fgn[idx])
}

export const asInt = n => function (fgn) {
  if (typeof fgn === "number") {
    return ((fgn | 0) === fgn)
      ? Right.create(fgn)
      : Left.create(Unexpected.create("Expecting integer, got a floating point number"));
  }
  console.log(n, fgn)
  return Left.create(Unexpected.create("Expecting interger, got " + typeof fgn));
}

export const asArray = function (fgn) {
    if (!Array.isArray(fgn)) {
        return Left.create(Unexpected.create("Expecting array, got " + typeof fgn));
    }
    return Right.create(fgn);
}