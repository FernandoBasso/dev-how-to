import { kangaroo } from './kangaroo_jumps_v1';

describe("kangaroo()", () => {
  it("cases for NO", () => {
    expect(kangaroo(0, 1, 1, 1)).toBe("NO");
    expect(kangaroo(0, 1, 1, 2)).toBe("NO");
    expect(kangaroo(9_999, 2, 10_000, 2)).toBe("NO");
  });

  it("cases for YES", () => {
    expect(kangaroo(0, 2, 1, 1)).toBe("YES");
    expect(kangaroo(0, 10, 50, 8)).toBe("YES");
    expect(kangaroo(9_996, 2, 10_000, 1)).toBe("YES");
  });
});
