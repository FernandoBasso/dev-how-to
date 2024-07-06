const log = console.log.bind(console);

function count(
  houseStart,
  houseEnd,
  treePos,
  fruits,
) {
  let cnt = 0;

  for (let i = 0; i < fruits.length; ++i) {
    let fruitPos = treePos + fruits[i];

    if (fruitPos >= houseStart && fruitPos <= houseEnd)
      ++cnt;
  }

  return cnt;
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
  let appleCount = count(houseStart, houseEnd, appleTree, apples);
  let orangeCount = count(houseStart, houseEnd, orangeTree, oranges);

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

