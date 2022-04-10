export const NAME = "l31b Related Type Operators";

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

type URLList = {
  [key in string]: URL;
};

//
// ‘extends’ indicates a constraint here. It means ‘Formats’
// must contain keys whose values are of the type ‘URL’.
//
// ‘format’ should not be whatever string in the infinite set
// of possible strings, but one of the available formats our
// function is expected to handle.
//
// Pay attention to the second generic type parameter (‘Key’)
// and how it makes use of the first generic type parameter
// (‘Formats’), and how we use ‘Key’ to annotate the return
// type.
//
async function loadFile<
  Formats extends URLList,
  Key extends keyof Formats,
>(
  fileFormats: Formats,
  format: Key,
): Promise<{ format: Key, loaded: boolean }> {
  const data = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
};

const result = await loadFile(videos, "format720p");
//
// The return type is:
//
//   { format: "format720p", loaded: boolean }
//
// Note how ‘format’ is the literal type "format720p", which
// is the format passed as parameter to the function!
//
