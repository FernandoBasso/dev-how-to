const log = console.log.bind(console);

function hasFallenIntoHouseRange(houseStart, houseEnd, fruitPos) {
  return fruitPos >= houseStart && fruitPos <= houseEnd;
}

function countFruits(
  houseStart,
  houseEnd,
  treePos,
  fruits,
) {
  return fruits.reduce(function countReducer(count, fruit) {
    const fruitPos = treePos + fruit;

    return hasFallenIntoHouseRange(houseStart, houseEnd, fruitPos)
      ? ++count
      : count;
  }, 0);
}

/**
 * Count how many apples and oranges fall within the range of the house.
 */
function countApplesAndOranges(
  houseStart,
  houseEnd,
  appleTree,
  orangeTree,
  apples,
  oranges,
) {
  let appleCount = countFruits(houseStart, houseEnd, appleTree, apples);
  let orangeCount = countFruits(houseStart, houseEnd, orangeTree, oranges);

  log(appleCount);
  log(orangeCount);
}

countApplesAndOranges(
  7,
  10,
  4,
  12,
  [4, 2, 6, -3, 9],
  [-2, 3, -1, -7, -4],
);

countApplesAndOranges(
  3,
  7,
  2,
  8,
  [],
  [],
);

