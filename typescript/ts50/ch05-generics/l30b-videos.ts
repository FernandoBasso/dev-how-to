export const NAME = "l30b Generic Constraints";

const log: Console["log"] = console.log.bind(console);

//
// We use the ‘extends’ keyword to constrain, or narrow down
// the generic so it can only be a subset of the specified
// types.
//

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


function isAvailable<Formats extends object>(
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
// OK. Array is still an object.
//
// The book sayids that we wouldn't be able to pass arrays,
// but clearly we can (at least with tsc 4.5.5).
//
isAvailable([], 0);

//
// But here the type-checker helps us out and tries to prevent
// us from passing parameters types that can't possibly work
// with the ‘in’ operator.
//
isAvailable("jedi", 0);
isAvailable(undefined, 5);

