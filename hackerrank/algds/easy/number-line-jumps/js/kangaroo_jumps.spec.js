import { kangaroo } from './kangaroo_jumps_v1';

describe("kangaroo", () => {
  test("same start location and same jump velocity", () => {
    expect(kangaroo(0, 1, 0, 1)).toBe("YES");
  });

  test("same start location and different jump velocity", () => {
    expect(kangaroo(0, 1, 0, 2)).toBe("NO");
  });

  test("same location but different velocities", () => {
    expect(kangaroo(0, 1, 0, 2)).toBe("NO");
    expect(kangaroo(0, 2, 0, 1)).toBe("NO");
  });
});
