export const name = "infer 01";

type UndefinedAsNull<T> =
  T extends undefined ? null : T;

type T1 = UndefinedAsNull<undefined>;
// Type is null.

type T2 = UndefinedAsNull<null>;
// Type is null.

type T3 = UndefinedAsNull<string>;
// Type is string.
