class Result{}
class Value extends Result
{
    constructor(value)
    {
        super();
        this.value = value;
    }

    toString()
    {
        return "Value(" + this.value + ")";
    }
}
class Call extends Result
{
    constructor(op, arg, continuation)
    {
        super();
        this.op = op;
        this.arg = arg;
        this.continuation = continuation;
    }

    toString()
    {
        return "Call(op:'" + this.op +"', arg:'" + this.arg + "')" + this.continuation;
    }
}

class HandlerClause
{
    constructor(op, effC)
    {
        this.op = op;
        this.effC = effC;
    }

    toString()
    {
        return "HandlerClause(op:'" + this.op +"')";
    }
}

class Handler
{
    constructor(effCs = [], valueClause = x => new Value(x), finallyClause = x => new Value(x))
    {
        this.effCs = effCs;
        this.finallyClause = finallyClause;
        this.valueClause = valueClause;
    }

    getHandleClause(result)
    {
        if(!result instanceof Result)
        {
            throw new Error("Cannot determine handling of a non-Result type!");
        }
        if(result instanceof Value)
        {
            return true;
        }
        return this.effCs.find(effC => effC.op === result.op);
    }
    
    toString()
    {
        return "Handler(effCs: '" + this.effCs.map(c => c.op).join(",")  + "')";
    }
}

const bind = function (result, cont)
{
    console.log(".bind | " + result + " >>= (" + cont + ")")
    if(result instanceof Value)
    {
        return cont(result.value);
    }
    else if (result instanceof Call)
    {
        return new Call(result.op, result.args, y => bind(result.continuation(y), cont))
    }
}

const evalWithoutFinally = function (result, handler)
{
    console.log("handle " + result + " with " + handler)
    if(result instanceof Value)
    {
        return handler.valueClause(result.value);
    }
    else if (result instanceof Call)
    {
        let h = handler.getHandleClause(result);
        if(!h)
        {
            return new Call(result.op, result.args, y => eval(result.continuation(y), handler))
        } 
        else
        {
            return h.effC(result.arg, y => eval(result.continuation(y), handler));
        }
    }
}

const eval = function(result, handler)
{
    return bind(evalWithoutFinally(result, handler), handler.finallyClause);
}


// pervasives

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

// basically the same as list.every
let forall = function(predicate, list)
{
    if(list === null || list.length === 0)
    {
        return true;
    }
    const [x, ...xs] = list;
    if(predicate(x))
    {
        return forall (predicate, xs);
    }
    return false;
}


// simple test function, returns Value(9)

const printEffect = "Print";

let testHandler = new Handler([
    new HandlerClause(printEffect, (args, k) => {
        console.log(args);
        return bind(k(1), x => bind(k(2+x), y => k(3+y)))
    })
]);

// boolean -> Result
let test = function (x)
{
    if(x > 0)
    {
        return new Value(42);
    }
    else
    {
        return new Call(printEffect, "Boom!", (y => {
            console.log(y);
            return new Value(y+1);
        }));
    }
}
// console.log("Result: '" + eval(test (-1), testHandler) + "'");
// console.log("Result: '" + eval(test (-1), new Handler()) + "'");

// 8 queen problem
/**
 * Select effect, takes no arguments and returns a boolean
 * effect Select : bool
 */
const selectEffect = "Select";

/**
 * Equivalent to 
 * type ChoiceResult = Failure | Success of (int*int) list
 */
class ChoiceResult{}
/**
 * Indicates failure when finding suitable coordinates for the 8 queens.
 */
class Failure extends ChoiceResult
{
    toString()
    {
        return "Failure";
    }
}
/**
 * Indicates that a solution has been found in the form of (int*int) list, in JS terms an array of 2-size arrays. 
 */
class Success extends ChoiceResult
{
    constructor(possibleCoordinates)
    {
        super();
        this.possibleCoordinates = possibleCoordinates;
    }
    
    toString()
    {
        return "Success(" + JSON.stringify(this.possibleCoordinates) + ")";
    }
}

let amb = new Handler([
    new HandlerClause(selectEffect, (_, k) =>
    {
        return bind(k(true), choiceResult => 
        {
            console.log("amb.bind | " + choiceResult);
            if(choiceResult instanceof Success)
            {
                return new Value(choiceResult);
            }
            else if (choiceResult instanceof Failure)
            {
                return k(false);
            }
        })
    })
]);

let selectFrom = function(possibleCoordinates)
{
    console.log(".selectFrom");
    console.log(possibleCoordinates);
    if(possibleCoordinates === null || possibleCoordinates.length === 0)
    {
        return new Value(new None());
    }
    const [x, ...xs] = possibleCoordinates;
    return new Call(selectEffect, null, y =>
        {
            if(y === true)
            {
                console.log("Continuing with true");
                return new Value(new Some(x));
            }
            console.log("Continuing with false");
            return selectFrom(xs);
        });
}

let noAttack = function(p0, p1)
{
    const [x0, y0] = p0;
    const [x1, y1] = p1;
    return x0 !== x1 && y0 !== y1 && Math.abs(x0 - x1) !== Math.abs(y0 - y1);
}

let available = function(row, queens)
{
    return [1,2,3,4,5,6,7,8].filter(y => queens.every(q => noAttack([row, y], q)));
}

let findQueens = function(x, queens)
{
    if(x >= 9 )
    {
        return new Value(new Success(queens));
    }

    return bind(selectFrom(available(x,queens)), selection =>
    {
        console.log("findQueens.bind | " + x);
        console.log("findQueens.bind | " + selection);
        if(selection instanceof None)
        {
            return new Value(new Failure());
        }
        let newQueens = [[x, selection.value]].concat(queens);
        console.log("findQueens.bind | ");
        console.log(newQueens);
        return findQueens(x + 1, newQueens);
    })
}

// console.log("Result: " + eval(findQueens(1, []), new Handler()));
console.log("Result: " + eval(findQueens(1, []), amb));


// another take on the 8 queens problem: BFS

/**
 * effect Get_next : ((bool -> result)*bool) option
 * effect Add_to_queue : ((bool -> result)*bool) -> unit
 */
/*const getNext = "GET_NEXT";
const addToQueue = "ADD_TO_QUEUE";

let queue = function(initialState)
{
    return new Handler(
        [
            new HandlerClause(getNext, (args, k) => 
            {
                return function(q)
                {
                    if(q === null || q.length === 0)
                    {
                        return k(new None(), []);
                    }
                    const [hd, ...tl] = q;
                    return k(new Some(hd), tl);
                }
            }),
            new HandlerClause(addToQueue, (args, k) =>
            {

            })
        ]
        );
}
*/



