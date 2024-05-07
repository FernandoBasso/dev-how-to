import { jest } from "@jest/globals";
import { cancellable } from "./cancellable_v1";

jest.useFakeTimers();

describe("cancellable()", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    jest.clearAllTimers();
  });

  it("should call fn once immediatelly", () => {
    const spyFn = jest.fn();
    const cancelFn = cancellable(spyFn, [1, "two"], 8);

    //
    // spyFn is called immediately once, no matter. Even if we
    // immediately call cancelFn(), the spy has already been
    // called once nonetheless. That is, cancelFn cannot hope
    // to prevent the first call of spyFn.
    //

    expect(spyFn).toHaveBeenCalledTimes(1);
    expect(spyFn).toHaveBeenCalledWith(1, "two");
  });

  it("should allow calling a few times then cancel it", () => {
    const spyFn = jest.fn();
    const cancelFn = cancellable(spyFn, ["hello"], 8);

    expect(spyFn).toHaveBeenCalledTimes(1);
    expect(spyFn).toHaveBeenCalledWith("hello");

    jest.advanceTimersByTime(8);

    expect(spyFn).toHaveBeenCalledTimes(2);
    expect(spyFn).toHaveBeenCalledWith("hello");

    jest.advanceTimersByTime(8);

    expect(spyFn).toHaveBeenCalledTimes(3);
    expect(spyFn).toHaveBeenCalledWith("hello");

    cancelFn();

    jest.advanceTimersByTime(8);

    //
    // Should not have called it again now, so the call count
    // should still be 3.
    //
    expect(spyFn).toHaveBeenCalledTimes(3);
  });

  it("should working when testing with setTimeout()", () => {
    const spyFn = jest.fn();
    const cancelFn = cancellable(spyFn, ["hello"], 8);

    setTimeout(() => {
      cancelFn();
    }, 32)

    //
    // Call once no matter what.
    //
    expect(spyFn).toHaveBeenCalledTimes(1);
    expect(spyFn).toHaveBeenCalledWith("hello");

    //
    // After 8 millis, should call again.
    //
    jest.advanceTimersByTime(8);
    expect(spyFn).toHaveBeenCalledTimes(2);
    expect(spyFn).toHaveBeenCalledWith("hello");

    //
    // After 8 more millis, should call again.
    //
    jest.advanceTimersByTime(8);
    expect(spyFn).toHaveBeenCalledTimes(3);
    expect(spyFn).toHaveBeenCalledWith("hello");

    //
    // Cancel the interval. No matter what, the spyFn should
    // never be called again from now on.
    //
    cancelFn();

    jest.advanceTimersByTime(8);

    //
    // Should not have called it again now, so the call count
    // should still be 3.
    //
    expect(spyFn).toHaveBeenCalledTimes(3);
    expect(spyFn).toHaveBeenCalledTimes(3);
  });
});
