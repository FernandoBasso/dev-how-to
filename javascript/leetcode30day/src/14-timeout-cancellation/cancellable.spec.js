import { jest } from "@jest/globals";
import { cancellable } from "./cancellable_v1";

jest.useFakeTimers();

describe("cancellable()", () => {
  it("should call fn() with correct params", async () => {
    const spy = jest.fn();

    const cancelSpy = cancellable(spy, [1e2, 1e3], 10);

    jest.advanceTimersByTime(20);

    // Called this too late. fn() (spy) has already been
    // called by now.
    cancelSpy();

    expect(spy).toHaveBeenCalledTimes(1);
    expect(spy).toHaveBeenCalledWith(1e2, 1e3);
  });

  it("should cancel fn() call", () => {
    const spy = jest.fn();

    const  cancelSpy = cancellable(spy, undefined, 10);

    // Cancel before the 10 milliseconds timeout. fn() (spy)
    // will not be called.
    cancelSpy();

    jest.advanceTimersByTime(20);

    expect(spy).toHaveBeenCalledTimes(0);
  });
});
