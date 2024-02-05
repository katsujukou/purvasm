import cbor from 'cbor';

export const decodeAllImpl = async function (buf) {
  return await cbor.decodeAll(buf);
}

export const decodeFirstImpl = async function (buf) {
    return await cbor.decodeFirst(buf);
}