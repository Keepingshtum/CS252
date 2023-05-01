// NOTE: This library uses non-standard JS features (although widely supported).
// Specifically, it uses Function.name.

function any(v) {
  return true;
}

function isNumber(v) {
  return !Number.isNaN(v) && typeof v === 'number';
}
isNumber.expected = "number";


function isBoolean(v) {
  return typeof v === 'boolean';
}
isBoolean.expected = "boolean";

function isDefined(v) {
  return v !== null && typeof v !== 'undefined';
}
isDefined.expected = 'defined';

function isString(v) {
  //Fun!
  return (Object.prototype.toString.call(v) === '[object String]') || typeof v === 'string';
}
isString.expected = 'string';

function isNegative(v) {
  return isNumber(v) && v < 0;
}
isNegative.expected = 'negative number';


function isPositive(v) {
  return isNumber(v) && v > 0;
}
isPositive.expected = 'positive number';


// Combinators:

function and() {
  let args = Array.prototype.slice.call(arguments);
  let contract = function (v) {
    for (let i in args) {
      if (!args[i].call(this, v)) {
        return false;
      }
    }
    return true;
  }
  contract.expected = expect(args[0]);
  for (let i = 1; i < args.length; i++) {
    contract.expected += " and " + expect(args[i]);
  }
  return contract;
}


function or() {
  let args = Array.prototype.slice.call(arguments);
  let contract = function (v) {
    for (let i in args) {
      if (args[i].call(this, v)) {
        return true;
      }
    }
    return false;
  }
  contract.expected = expect(args[0]);
  for (let i = 1; i < args.length; i++) {
    contract.expected += " or " + expect(args[i]);
  }
  return contract;
};

function not() {
  let args = Array.prototype.slice.call(arguments);
  let contract = function (v) {
    return !args[0].call(this, v);
  }
  contract.expected = "not " + expect(args[0]);
  return contract;
};



// Utility function that returns what a given contract expects.
function expect(f) {
  // For any contract function f, return the "expected" property
  // if it is specified.  (This allows developers to specify what
  // the expected property should be in a more readable form.)
  if (f.expected) {
    return f.expected;
  }
  // If the function name is available, use that.
  if (f.name) {
    return f.name;
  }
  // In case an anonymous contract is specified.
  return "ANONYMOUS CONTRACT";
}


function contract(preconditionsList, results, contractFunction) {
  let handler = {
    apply: function (thisFunc, thisObj, thisArg) {
      for (let k in thisArg) {
        let preconditionsValid = preconditionsList[k].call(thisObj, thisArg[k]);
        if (!preconditionsValid) {
          throw { "message": "Contract violation in position " + k + ". Expected " + preconditionsList[k].expected + " but received " + thisArg[k] + ".  Blame -> Top-level code" };
        }
      }

      let actualResults = thisFunc.apply(thisObj, thisArg);
      if (!results(actualResults)) {
        throw { "message": "Contract violation. Expected " + results.expected + " but returned " + actualResults + ". Blame -> " + contractFunction.name };
      }

      return actualResults;
    }
  };

  let prox = new Proxy(contractFunction, handler);
  return prox;
}

module.exports = {
  contract: contract,
  any: any,
  isBoolean: isBoolean,
  isDefined: isDefined,
  isNumber: isNumber,
  isPositive: isPositive,
  isNegative: isNegative,
  isInteger: Number.isInteger,
  isString: isString,
  and: and,
  or: or,
  not: not,
};
