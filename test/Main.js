exports.exit = function(code) {
  return function() {
    process.exit(code);
  };
};
