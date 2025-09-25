Uint8Array.prototype.decodedText = function (
  this: Uint8Array,
) {
  return new TextDecoder().decode(this);
};

const text = new TextEncoder().encode("HELLO").decodedText();
console.log(text);
