import { jest } from "@jest/globals";
import { delay } from "./sleep_v1";

jest.useFakeTimers();

describe("delay()", () => {
  beforeEach(() => {
    jest.resetAllMocks();
  });

  it("should delay 64 milliseconds", async () => {
    const spy = jest.spyOn(global, "setTimeout");

    delay(64);

    expect(spy).toHaveBeenCalledTimes(1);
    expect(spy).toHaveBeenCalledWith(expect.any(Function), 64);
  });

  it("should delay 128 milliseconds", async () => {
    const spy = jest.spyOn(global, "setTimeout");

    delay(128);

    expect(spy).toHaveBeenCalledTimes(1);
    expect(spy).toHaveBeenCalledWith(expect.any(Function), 128);
  });
});
