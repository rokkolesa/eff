// core JavaScript pervasives

class Call {
    constructor(op, arg, continuation = x => x) {
        this.op = op;
        this.arg = arg;
        this.continuation = continuation;
    }

    toString() {
        return `Call(op:'${this.op}', arg:'${JSON.stringify(this.arg)}')`;
    }
}

class HandlerClause {
    constructor(op, handler) {
        this.op = op;
        this.handler = handler;
    }

    toString() {
        return `HandlerClause(op:'${this.op}')`;
    }
}

class Handler {
    constructor(
        effectClauses = [],
        valueClause = x => x,
        finallyClause = x => x
    ) {
        this.effectClauses = effectClauses;
        this.finallyClause = finallyClause;
        this.valueClause = valueClause;
    }

    getEffectClause(result) {
        if (result instanceof Call) {
            return this.effectClauses
                .find(effectClause => effectClause.op === result.op);
        }
    }

    toString() {
        return `Handler(effectClauses: '${JSON.stringify(this.effectClauses.map(effectClause => effectClause.effect))}')`;
    }
}

const bind = function (result, continuation) {
    if (!(result instanceof Call)) {
        return continuation(result);
    }
    return new Call(
        result.op,
        result.arg,
        y => bind(result.continuation(y), continuation)
    )
}

const evalWithoutFinally = function (handler, result) {
    if (!(result instanceof Call)) {
        return handler.valueClause(result);
    }
    let effectClause = handler.getEffectClause(result);
    if (effectClause) {
        return effectClause.handler(
            result.arg,
            y => evalWithoutFinally(handler, result.continuation(y))
        );
    }
    return new Call(
        result.op,
        result.arg,
        y => evalWithoutFinally(handler, result.continuation(y))
    );
}

const eval = function (handler, result) {
    return bind(
        evalWithoutFinally(handler, result),
        handler.finallyClause
    );
}

const top_eval = function (result) {
    if (!(result instanceof Call)) {
        return result;
    }
    if (result.op === 'Print') {
        console.log(result.arg);
        return top_eval(result.continuation());
    }
    if (result.op === 'RandomInt') {
        const rnd = Math.floor(Math.random() * Math.floor(result.arg));
        return top_eval(result.continuation(rnd));
    }
    if (result.op === 'RandomFloat') {
        const rnd = Math.random() * result.arg;
        return top_eval(result.continuation(rnd));
    }
    if (result.op === 'Read') {
        const value = prompt("Enter value");
        return top_eval(result.continuation(value));
    }
    throw `Uncaught effect ${result.op} ${JSON.stringify(result.arg)}`;
}

class ArbitraryPattern {
    satisfies() {
        return true;
    }
}

class ConstantPattern {
    constructor(value) {
        this.value = value;
    }

    satisfies(value) {
        return this.value === value;
    }
}

class VariantPattern {
    constructor(value, shapes = []) {
        this.value = value;
        this.shapes = shapes;
    }

    satisfies(value) {
        return this.value === value.name
            && this.shapes.every(shape => shape.satisfies(value.arg));
    }
}

class TuplePattern {
    constructor(shapes = []) {
        this.shapes = shapes;
    }

    satisfies(value) {
        return this.shapes.length === Object.keys(value).length
            && this.shapes.every((s, i) => s.satisfies(value[i]));
    }
}

class RecordPattern {
    constructor(shapes = {}) {
        this.shapes = shapes;
    }

    satisfies(value) {
        return Object.keys(this.shapes).every(k => this.shapes[k].satisfies(value[k]));
    }
}
// end core JavaScript pervasives


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

// generating a seuqence
const computeEffect = "Compute";

let computeHandler = new Handler([
    new HandlerClause(computeEffect, (args, k) => {
        const msg = args[0];
        const x   = args[1];
        console.log(msg);
        return bind(
            k(x),
            y   => k(x * y)
        )
    })
]);

// boolean -> Result
let generate = function (x)
{
    if(x < 0)
    {
        return 42;
    }
    else
    {
        return new Call(computeEffect, ["Computing...", x], (y => {
            console.log(`Step: ${y}`);
            return y + 1;
        }));
    }
}
console.log(`Result: '${JSON.stringify([...Array(11).keys()].map(i => top_eval(eval(computeHandler, generate(i)))))}'`);
// console.log("Result: '" + top_eval(eval(new Handler(), generate (-1))) + "'");

// simple test function, returns Value(9)

const errorEffect = "Error";

let errorHandler = new Handler([
    new HandlerClause(errorEffect, (args, k) => {
        const msg = args[0];
        const x   = args[1];
        console.log(msg);
        return k (-x);
    })
]);

// boolean -> Result
let test = function (x)
{
    let condition = x > 0;
    if(new ConstantPattern(false).satisfies(condition))
    {
        return new Call(errorEffect, ["Boom!", x]);
    }
    if(new ArbitraryPattern().satisfies(condition))
    {
        return 42;
    }
}
// console.log(`Result: '${top_eval(eval(errorHandler, test (-3)))}'`);
// console.log(`Result: '${top_eval(eval(errorHandler, test (-4)))}'`);
// console.log("Result: '" + top_eval(eval(new Handler(), test (-1))) + "'");

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
            if(choiceResult instanceof Success)
            {
                return choiceResult;
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
    if(possibleCoordinates === null || possibleCoordinates.length === 0)
    {
        return new None();
    }
    const [x, ...xs] = possibleCoordinates;
    return new Call(selectEffect, null, y => y === true ? new Some(x) : selectFrom(xs));
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
        return new Success(queens);
    }

    return bind(selectFrom(available(x,queens)), selection =>
    {
        if(selection instanceof None)
        {
            return new Failure();
        }
        let newQueens = [[x, selection.value]].concat(queens);
        return findQueens(x + 1, newQueens);
    })
}

// console.log("Result: " + top_eval(eval(new Handler(), findQueens(1, []))));
// console.log("Result: " + top_eval(eval(amb, findQueens(1, []))));


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



