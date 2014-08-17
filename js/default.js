String.prototype.contains = function(str) {
  return this.indexOf(str) > -1;
};

String.prototype.endsWith = function(suffix) {
  return this.indexOf(suffix, this.length - suffix.length) !== -1;
};
