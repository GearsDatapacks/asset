export function slice(string, start, end) {
  if (end === -1) {
    return string.slice(start);
  } else {
    return string.slice(start, end);
  }
}