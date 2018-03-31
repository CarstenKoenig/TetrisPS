// module SignalExt

// HACK:
// need to reintroduce this as it's not exportet
// from the signal module
function make(initial) {
  var subs = [];
  var val = initial;
  var sig = {
    subscribe: function (sub) {
      subs.push(sub);
      sub(val);
    },
    get: function () { return val; },
    set: function (newval) {
      val = newval;
      subs.forEach(function (sub) { sub(newval); });
    }
  };
  return sig;
};


exports.foldEff = function (fun) {
  return function (seed) {
    return function (sig) {
      return function () {
        var acc = seed;
        var out = make(acc);
        sig.subscribe(function (val) {
          acc = fun(val)(acc)();
          out.set(acc);
        });
        return out;
      };
    };
  };
};