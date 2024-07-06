const log = console.log.bind(console);

function countFruits(
  houseStart,
  houseEnd,
  treePos,
  fruits,
) {
  let count = 0;

  for (const fruit of fruits) {
    let fruitPos = treePos + fruit;

    if (fruitPos >= houseStart && fruitPos <= houseEnd)
      ++count;
  }

  return count;
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
  [100, 101, 105],
  [200, 1050, -77],
);

