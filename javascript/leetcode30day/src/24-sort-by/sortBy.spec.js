import { sortBy } from "./sortBy_v1";

describe("sortBy()", () => {
  it("should sort in ascending order by the element value", () => {
    const arr = [5, 4, 1, 2, 3];
    const getValueToSortBy = x => x;
    const expected = [1, 2, 3, 4, 5];

    expect(sortBy(arr, getValueToSortBy)).toEqual(expected);
  });

  it("should sort in ascending order by the property value", () => {
    const arr = [{ val: 1 }, { val: 3 }, { val: 2 }];
    const getValueToSortBy = obj => obj.val;
    const expected = [{ val: 1 }, { val: 2 }, { val: 3 }];

    expect(sortBy(arr, getValueToSortBy)).toEqual(expected);
  });

  it("should sort in ascending order by tuple second element", () => {
    const arr = [[3, 4], [5, 2], [10, 1]];
    const getValueToSortBy = tuple => tuple[1];
    const expected = [[10, 1], [5, 2], [3, 4]];

    expect(sortBy(arr, getValueToSortBy)).toEqual(expected);
  });
});
