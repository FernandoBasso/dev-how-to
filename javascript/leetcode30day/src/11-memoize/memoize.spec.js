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

  it("can be tested without the spy", () => {
    let callCount = 0;

    const memoizedAdd = memoize(function add(x, y) {
      ++callCount;

      return x + y;
    });

    memoizedAdd(1, 2);
    expect(callCount).toEqual(1);

    ////
    // Call again with the same input. Should return previously
    // computed value without calling the add again.
    //
    memoizedAdd(1, 2);
    expect(callCount).toEqual(1);
    //
    // If callCount is still 1, it really wasn't called a second
    // time, which proves the memoization is taking place.
    ////
  });
});
