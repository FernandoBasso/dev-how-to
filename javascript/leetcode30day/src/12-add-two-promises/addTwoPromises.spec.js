import { addTwoPromises } from "./addTwoPromises_v1";


describe("addTwoPromises()", () => {
  it("should simply add the results of the two promises", async () => {
    const p1 = new Promise(resolve => setTimeout(() => resolve(-1), 15));
    const p2 = new Promise(resolve => setTimeout(() => resolve(1), 10));

    const result = await addTwoPromises(p1, p2);
    expect(result).toEqual(0);
  });
});
