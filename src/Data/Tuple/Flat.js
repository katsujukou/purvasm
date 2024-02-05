import { Tuple } from "../Data.Tuple/index.js";

export function mkT2 (a) {
    return function (b) {
        return [a, b];
    }
}

export function mkT3 (a) {
    return function (b) {
        return function (c) {
            return [a, b, c];
        };
    };
}

export function toTuple2 (t2) {
    return Tuple.create(t2[0])(t2[1]);
}

export function toTuple3 (t3) {
    const [a, b, c] = t3;
    return Tuple.create(a)(Tuple.create(b)(c));
}