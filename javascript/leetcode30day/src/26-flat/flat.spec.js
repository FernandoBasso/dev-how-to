import { flat } from "./flat_v1";

describe("flat()", () => {
  it("should return the original array when n = 0", () => {
    expect(
      flat([1, [2, [3]]], 0)
    ).toEqual([1, [2, [3]]]);
  });

  it("should flatten first level when n = 1", () => {
    expect(
      flat([1, [2, [3]]], 1)
    ).toEqual([1, 2, [3]]);
  });
});
