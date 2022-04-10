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

function isFormatAvailable(
  obj: VideoFormatURLs,
  key: string,
): key is keyof VideoFormatURLs {
  return key in obj;
}

function isSubtitleAvailable(
  obj: SubtitleURLs,
  key: string,
): key is keyof SubtitleURLs {
  return key in obj;
}

function loadFormat(format: string): void {
  if (isFormatAvailable(videos, format)) {
    const x = videos[format];
    log(x);
  }
};

//
// Note that ‘isFormatAvailable’ and ‘isSubtitleAvailable’
// have the same implementation. We are violating the single
// responsibility principle.
//
