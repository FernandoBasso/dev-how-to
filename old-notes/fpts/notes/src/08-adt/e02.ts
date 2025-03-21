//
// tags: option sum-type adt
//

//
// adt = algebraic data types
//
// We want to stringify a type Option<boolean>. It can either be "none"
// or a boolean "true" or "false".
//
// We match None or Some().
//
//
// Pattern match should cover all cases so we end up with _total_
// functions.
//
// FP languages have syntax for pattern matching in JS/TS, we need other
// approaches (in 2024, there is a stage 1 proposal for patter
// matching).
//
