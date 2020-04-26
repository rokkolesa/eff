// core JavaScript pervasives

class Result { }
class Value extends Result {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() {
        return "Value(" + this.value + ")";
    }
}
class Call extends Result {
    constructor(op, arg, continuation) {
        super();
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
    constructor(effCs = [], valueClause = x => new Value(x), finallyClause = x => new Value(x)) {
        this.effCs = effCs;
        this.finallyClause = finallyClause;
        this.valueClause = valueClause;
    }

    getHandleClause(result) {
        if (!result instanceof Result) {
            throw new Error("Cannot determine handling of a non-Result type!");
        }
        if (result instanceof Value) {
            return true;
        }
        return this.effCs.find(effC => effC.op === result.op);
    }

    toString() {
        return "Handler(effCs: '" + this.effCs.map(c => c.op).join(",") + "')";
    }
}

const bind = function (result, cont) {
    console.log(".bind | " + result + " >>= (" + cont + ")")
    if (result instanceof Value) {
        return cont(result.value);
    }
    else if (result instanceof Call) {
        return new Call(result.op, result.args, y => bind(result.continuation(y), cont))
    }
}

const evalWithoutFinally = function (result, handler) {
    console.log("handle " + result + " with " + handler)
    if (result instanceof Value) {
        return handler.valueClause(result.value);
    }
    else if (result instanceof Call) {
        let h = handler.getHandleClause(result);
        if (!h) {
            return new Call(result.op, result.args, y => eval(result.continuation(y), handler))
        }
        else {
            return h.effC(result.arg, y => eval(result.continuation(y), handler));
        }
    }
}

const eval = function (result, handler) {
    return bind(evalWithoutFinally(result, handler), handler.finallyClause);
}

// end core JavaScript pervasives
