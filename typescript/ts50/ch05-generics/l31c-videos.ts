export const NAME = "l31c Related Type Operators";

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

type PossibleKeys = "meetup" | "conference" | "hackathon" | "webinar";

type Groups = {
  [key in PossibleKeys]: unknown;
};

type URLObject = {
  [key in string]: URL;
};

type Loaded<Key> = {
  format: Key,
  loaded: boolean,
};

async function loadFile<
  Formats extends URLObject,
  Key extends keyof Formats,
>(
  fileFormats: Formats,
  format: Key,
): Promise<Loaded<Key>> {
  const data = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
};

const r1 = await loadFile(videos, "format720p");
const r2 = await loadFile(videos, "format1080p");

var foo: Loaded<"hey"> = {
  loaded: !!1,
  format: "Hey",
};
