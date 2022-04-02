export const NAME = "l30a Generic Constraints";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type SubtitleURLs = {
  german: URL,
  french: URL,
  english: URL,
};

declare const videos: VideoFormatURLs;

function isAvailable<Formats>(
  obj: Formats,
  key: string | number | symbol ,
): key is keyof Formats {
  return key in obj;
}

function loadFormat(format: string): void {
  if (isAvailable(videos, format)) {
    const x = videos[format];
    log(x);
  }
};

//
// Passing an array is still OK:
//
isAvailable(["hello", "world"], 0);
// → true

isAvailable(["hello", "world"], "1");
// → true

isAvailable(["hello", "world"], 2);
// → false

isAvailable(["hello", "world"], "3");
// → false

//
// BEWARE: Unfortunately, we can also invoke ‘isAvailable()’
// with things we shouldn't.
//
// These are all invalid:
//
isAvailable(null, "foo");
isAvailable(undefined, "bar");
isAvailable("hello", 1);

//
// > '0' in "hello"
// Uncaught TypeError: Cannot use 'in' operator to search for '0' in hello
// > "foo" in null;
// Uncaught TypeError: Cannot use 'in' operator to search for 'foo' in null
// > "bar" in undefined;
// Uncaught TypeError: Cannot use 'in' operator to search for 'bar' in undefined
// > 1 in "hello"
// Uncaught TypeError: Cannot use 'in' operator to search for '1' in hello
//

