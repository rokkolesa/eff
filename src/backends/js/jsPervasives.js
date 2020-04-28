// core JavaScript pervasives

class Call {
    constructor(op, arg, continuation) {
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
    console.log(".bind | " + result + " >>= (" + cont + ")")
    if (result instanceof Call) {
        return new Call(result.op, result.args, y => bind(result.continuation(y), cont))
    }
    return cont(result);
}

const evalWithoutFinally = function (result, handler) {
    console.log("handle " + result + " with " + handler);
    if (result instanceof Call) {
        let clause = handler.getHandleClause(result);
        if (clause) {
            return clause.effC(result.arg, y => eval(result.continuation(y), handler));
        }
        return new Call(result.op, result.args, y => eval(result.continuation(y), handler))
    }
    return handler.valueClause(result);
}

const eval = function (result, handler) {
    return bind(evalWithoutFinally(result, handler), handler.finallyClause);
}

// end core JavaScript pervasives
