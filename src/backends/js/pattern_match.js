class Maybe{}
class None extends Maybe{
    toString()
    {
        return "None";
    }
}
class Some extends Maybe
{
    constructor(value)
    {
        super();
        this.value = value;
    }

    toString()
    {
        return "Some("+this.value+")";
    }
}

class Expression
{
    constructor(label, expressions)
    {
        this.label = label;
        this.expressions = expressions;
    }
}

class Pattern
{
    constructor(label, patterns)
    {
        this.label = label;
        this.patterns = patterns;
    }
}

// type Sample = None | One of int | Both of int * Maybe int
// let test =
//   | None -> 0
//   | One x -> x
//   | Both x (Some y) -> x + y
//   | Both x (None) -> -x


let satisfies = function(pattern, expression)
{
    
}

let match = function(pattern, expression)
{
    if(pattern instanceof Parameter)
    {
        return expression;
    }
    return 
}

let test = function(match_var)
{
    if(satisfies(new Pattern('None'), match_var))
    {
        return 0;
    }
    if(satisfies(new Pattern('One', new Parameter('x')), match_var))
    {
        const x = match_var.expression[0];
        return x;
    }
    if(satisfies(new Pattern(
        'Both', 
        new Parameter('x'), 
        new Pattern(
            'Some', 
            new Parameter('y'))), match_var))
    {
        const x = match_var.expression[0];
        const y = match_var.expression[1];
        return x + y;
    }
    if(satisfies(new Pattern(
        'Both', 
        new Parameter('x'), 
        new Pattern(
            'None')), match_var))
    {
        const x = match_var.expression[0];
        return -x;
    }
}
test(new Expression("One", new Const(42)));


// Unifiation LIB - okay for basic pattern matching, but probably not okay for 
var unification = require('./node_modules/junify/lib/unification');
var $ = unification.variable;

let match_unify = function () {
    var unify = unification.unify;
    var slice = Array.prototype.slice;
    
    function match_aux(patterns, value) {
        var i, result, length;
    
        for (i = 0; i < patterns.length; i += 1) {
          length = patterns[i].length;
    
          // we only try to match if the match array contains at
          // least two items and the last item is a function
          if (length >= 2 &&
            typeof patterns[i][length - 1] === 'function') {
            result = unify(patterns[i].slice(0, length - 1),
                     value);
            if (result) {
              return patterns[i][length - 1](result);
            }
          }
        }
        return undefined;
      }
      return function () {
        var args = slice.apply(arguments);
        return function () {
          return match_aux(args, slice.apply(arguments));
        };
      };
    }();
    
    var fact = match_unify(
        [0, function() { return 1; }],
        [$('n'), function(r) { return r.n * fact(r.n - 1); }]
      );

//   console.log(fact(4));

