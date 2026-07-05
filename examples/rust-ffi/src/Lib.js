// The JS-backend twin of foreign/src/lib.js (dual-target, ADR-0038): same shape — an
// Effect is a curried thunk — same observable behaviour class (both sides are random).
export const rand = (max) => () => Math.floor(Math.random() * max) + 1;

export const shuffle = (arr) => () => {
  const shuffled = [...arr];
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
  return shuffled;
};
