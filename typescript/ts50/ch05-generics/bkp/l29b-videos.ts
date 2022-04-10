export const NAME = "l29b Video Formats";

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

//
// We can remove the ‘isFormatAvailable()’ and
// ‘isSubtitleAvailable()’ functions and use the generic one
// below.
//

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
// Note we called ‘isAvailable()’ without explicitly providing
// the generic type parameter. It works because TypeScript is
// able to infer that ‘videos’ is of the type
// ‘VideoFormatURLs’ and uses it as if the generic type
// parameter was passed with that type.
//

declare const videoFormats: VideoFormatURLs;
declare const subtitles: SubtitleURLs;

isAvailable<VideoFormatURLs>(videoFormats, "format1080p");
isAvailable<SubtitleURLs>(subtitles, "german");

//
// The above two lines are the same as this:
//
isAvailable(videoFormats, "format1080p");
isAvailable(subtitles, "german");

