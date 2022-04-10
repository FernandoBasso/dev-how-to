export const NAME = "l32a Generic Mapped Types";

const log: Console["log"] = console.log.bind(console);

type VideoFormatURLs = {
  format360p: URL;
  format480p: URL;
  format720p: URL;
  format1080p: URL;
};

type SubtitleURLs = {
  german: URL;
  french: URL;
  english: URL;
};

declare const videos: VideoFormatURLs;

type HD = Pick<VideoFormatURLs, "format720p" | "format1080p">;

type Split1 = keyof VideoFormatURLs;

type Split2 =
  | "format360p"
  | "format480p"
  | "format720p"
  | "format1080p";

//
// We also know that a mapped type runs over all keys and
// creates a new object with those keys. The following
// example creates the same type as VideoFormatURLs, but
// with the key also being the value:
//
type Split3 = {
  [Prop in keyof VideoFormatURLs]: Prop;
};

//
// Instead of getting the left side of an object type – the
// property keys – in union, we get the right side of an
// object type – the property types – in union.
//
type Split4 = {
  [Prop in keyof VideoFormatURLs]: Prop;
}[keyof VideoFormatURLs];

type Split5 = {
  [Prop in keyof VideoFormatURLs]: Record <Prop, VideoFormatURLs[Prop]>;
}[keyof VideoFormatURLs];

//
// ‘Split’ would probably be more well named if we choose
// something like ‘ToUnion’ perhaps.
//
type Split<Obj> = {
  [Prop in keyof Obj]: Record<Prop, Obj[Prop]>;
}[keyof Obj];

var s1: Split<VideoFormatURLs> = { "format480p": new URL("/videos")}

type AvailableFormats = Split<VideoFormatURLs>;

const hq: AvailableFormats = {
  format720p: new URL("..."),
  format1080p: new URL("..."),
};

const lofi: AvailableFormats = {
  format360p: new URL("..."),
  format480p: new URL("..."),
};
