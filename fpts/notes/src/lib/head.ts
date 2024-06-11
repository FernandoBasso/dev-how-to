const log: Console["log"] = console.log.bind(console);

import type { Nullable } from "./types";

function head(xs: string): string | undefined;
function head<T>(xs: T[]): T | undefined;
function head<T>(xs: string | T[]): T | string | undefined {
  return xs[0];
}

const a: Nullable<number> = head([1, 2, 3]);
const b: Nullable<string> = head(["may", "the", "force"]);

log({ a, b });

export { head };
