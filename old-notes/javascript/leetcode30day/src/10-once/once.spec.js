import { once } from "./once_v4";

describe("once()", () => {
  it("should call only once", () => {
    const f = x => x;
    const onceF = once(f);

    expect(onceF("hello")).toEqual("hello");
    expect(onceF("world")).toBe(undefined);
    expect(onceF("try")).toBe(undefined);
    expect(onceF("again")).toBe(undefined);

    const add1 = n => n + 1;
    const incrementOnce = once(add1);

    expect(incrementOnce(3)).toEqual(4);
    expect(incrementOnce(0)).toBe(undefined);
    expect(incrementOnce(42)).toBe(undefined);
  });
});
