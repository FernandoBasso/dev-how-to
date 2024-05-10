import { flat } from "./flat_v1";

describe("flat()", () => {
  it("should return the original array when n = 0", () => {
    expect(
      flat([1, [2, [3]]], 0)
    ).toEqual([1, [2, [3]]]);
  });

  it("should return the empty array if input is empty", () => {
    expect(flat([], 1)).toEqual([]);
  });

  it("should flatten first level when n = 1", () => {
    expect(
      flat([1, [2, [3]]], 1)
    ).toEqual([1, 2, [3]]);
  });

  it("should flatten first level when n = 2", () => {
    expect(
      flat([1, [2, [3]]], 2)
    ).toEqual([1, 2, 3]);
  });

  it("should work from leetcode test", () => {
    expect(
      flat([1, [2], [3, [4], 5], [6], 7], 2)
    ).toEqual([1, 2, 3, 4, 5, 6, 7]);

    // expect(
    //   flat([1, [4],[7, [9],12],[13,]], 2)
    // ).toEqual([1,4,7,9,12,13]);
  });
});
