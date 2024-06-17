//
// tags: list linked-list
//

const log: Console["log"] = console.log.bind(console);

type Nil = {
  readonly __tag: "Nil";
};

type Cons<T> = {
  readonly __tag: "Cons";
  readonly head: T;
  readonly tail: List<T>;
};

type List<T> = Nil | Cons<T>;

const nil: List<never> = { __tag : "Nil" };

function cons<T>(head: T, tail: List<T>): List<T> {
  return {
    __tag: "Cons",
    head,
    tail,
  };
}

function isNil<T>(xs: List<T>): xs is Nil {
  return xs.__tag === "Nil";
}

const xs1: List<number> = nil;
const xs2: List<number> = cons(1, cons(2, cons(3, nil)));

log(isNil(xs1));
log(isNil(xs2));


/**
 * Show list in string representation.
 *
 * We look ahead to decide if we should display one more ", " or not.
 */
function showList<T>(xs: List<T>): string {
  return isNil(xs)
    ? ''
    : `${xs.head}` + (isNil(xs.tail) ? '' : ', ' + showList(xs.tail))
}

log(showList(xs2));

export {};
