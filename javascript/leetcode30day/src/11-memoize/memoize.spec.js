import { jest } from "@jest/globals";
import { memoize } from "./memoize_v1";

describe("memoize()", () => {
  it("returns the correct result on first invocation", () => {
    const sum = (...args) => args.reduce((acc, n) => acc + n);

    expect(memoize(sum)(1, 3, 5)).toEqual(9);
  });

  it("memoizes an example sum function", () => {
    const sumSpy = jest.fn(() => {
      return 1;
    });

    const memoizedSum = memoize(sumSpy);
    expect(memoizedSum(1)).toEqual(1);
    expect(memoizedSum(1)).toEqual(1);
    expect(sumSpy).toHaveBeenCalledTimes(1);
  });
});
