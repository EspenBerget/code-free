expr = {
    type: "add",
    left: {
        type: "value",
        value: 2
    },
    right: {
        type: "mul",
        left: {
            type: "value",
            value: 3
        },
        right: {
            type: "value",
            value: 5
        }
    }
}

// Straight forward implementation
function eval(ast) {
    if (ast.type === "value") {
        return ast.value;
    }

    let left = eval(ast.left);
    let right = eval(ast.right);

    if (ast.type === "mul") {
        return left * right;
    } else if (ast.type === "add") {
        return left + right;
    }
}

console.log("eval:", eval(expr)); // => 17


// FIX
exprFix = {
    type: "fix",
    item: {
        type: "add",
        left: {
            type: "fix",
            item: {
                type: "value",
                value: 2
            }
        },
        right: {
            type: "fix",
            item: {
                type: "mul",
                left: {
                    type: "fix",
                    item: {
                        type: "value",
                        value: 3
                    }
                },
                right: {
                    type: "fix",
                    item: {
                        type: "value",
                        value: 5
                    }
                }
            }
        }
    }
}

function unFix(fix) {
    return fix.item;
}

function mapExpr(fn, ast) {
    if (ast.type !== "value") {
        ast.left = fn(ast.left);
        ast.right = fn(ast.right);
    }

    return ast;  // where recursion stops in foldFix
}

function foldFix(f, fix) {
    const go = (iter) => {
        return f(mapExpr(go, unFix(iter)));
    };
    return go(fix);
}

// AST -> Int
function algebra(ast) {
    switch (ast.type) {
        case "value":
            return ast.value;
        case "add":
            return ast.left + ast.right;
        case "mul":
            return ast.left * ast.right;
    }
}

function evalFix(ast) {
    return foldFix(algebra, ast);
}

console.log("evalFix:", evalFix(exprFix)); // => 17

// FREE
exprFree = {
    type: "free",
    item: {
        type: "add",
        left: {
            type: "pure",
            item: 2
        },
        right: {
            type: "free",
            item: {
                type: "mul",
                left: {
                    type: "pure",
                    item: 3
                },
                right: {
                    type: "pure",
                    item: 5
                }
            }
        }
    }
}


function unFree(free) {
    return free.item;
}

/* copy mapExpr and algebra from fix. js is dynamic so they still work */

function foldFree(phi, free) {
    if (free.type === "pure") {
        return unFree(free);
    }
    // otherwise free.type === "free"
    return phi(mapExpr(x => foldFree(phi, x), unFree(free)));
}

function evalFree(ast) {
    return foldFree(algebra, ast);
}

console.log("evalFree: ", evalFree(exprFree)); // => 17