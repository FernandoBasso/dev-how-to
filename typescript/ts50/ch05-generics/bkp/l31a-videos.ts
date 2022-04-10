export const NAME = "l31a Related Type Operators";

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
async function loadFile<Formats extends URLList>(
  fileFormats: Formats,
  format: keyof Formats,
): Promise<{ format: keyof Formats, loaded: boolean }> {
  const data = await fetch(fileFormats[format].href);

  return {
    format,
    loaded: data.status === 200,
  };
};

const file = await loadFile(videos, "format720p");
//
// Type of ‘file’ is
//
//   { format: keyof Formats, loaded: boolean }
//
// And ‘keyof Formats’ should be
//
//   format360p | format480p | format720p | format1080p
//
//
// But shouldn’t we know more? We are explicitly passing
// "format1080p" as second argument. We’ve already narrowed down
// the union through usage to a single value type. Why can’t
// result be of type
//
//   { format: "format1080p", loading: boolean }
//
// ?
//
