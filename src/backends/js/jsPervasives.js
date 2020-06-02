// core JavaScript pervasives

class Call {
    constructor(op, arg, continuation = x => x) {
        this.op = op;
        this.arg = arg;
        this.continuation = continuation;
    }

    toString() {
        return "Call(op:'" + this.op + "', arg:'" + this.arg + "')" + this.continuation;
    }
}

class HandlerClause {
    constructor(op, effC) {
        this.op = op;
        this.effC = effC;
    }

    toString() {
        return "HandlerClause(op:'" + this.op + "')";
    }
}

class Handler {
    constructor(effCs = [], valueClause = x => x, finallyClause = x => x) {
        this.effCs = effCs;
        this.finallyClause = finallyClause;
        this.valueClause = valueClause;
    }

    getHandleClause(result) {
        if (result instanceof Call) {
            return this.effCs.find(effC => effC.op === result.op);
        }
    }

    toString() {
        return "Handler(effCs: '" + this.effCs.map(c => c.op).join(",") + "')";
    }
}

const bind = function (result, cont) {
    if (result instanceof Call) {
        return new Call(result.op, result.arg, y => bind(result.continuation(y), cont))
    }
    return cont(result);
}

const evalWithoutFinally = function (handler, result) {
    if (result instanceof Call) {
        let clause = handler.getHandleClause(result);
        if (clause) {
            return clause.effC(result.arg, y => evalWithoutFinally(handler, result.continuation(y)));
        }
        return new Call(result.op, result.arg, y => evalWithoutFinally(handler, result.continuation(y)))
    }
    return handler.valueClause(result);
}

const eval = function (handler, result) {
    return bind(evalWithoutFinally(handler, result), handler.finallyClause);
}

const top_eval = function (result) {
    if (result instanceof Call) {
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
        throw `Uncaught effect ${result.op} ${result.arg}`;
    }
    return result;

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
            && this.shapes.every(s => s.satisfies(value.arg));
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
