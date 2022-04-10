export const NAME = "l33b Mapped Type Modifiers";

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

//
// ‘Split’ would probably be more well named if we choose
// something like ‘ToUnion’ perhaps.
//
type Split<Obj> = {
  [Prop in keyof Obj]: Record<Prop, Obj[Prop]>;
}[keyof Obj];

type AvailableFormats = Split<VideoFormatURLs>;

const s1: AvailableFormats = { format480p: new URL("/videos") };

type UserPrefs = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

const defaultUserPrefs: UserPrefs = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

//
// We only store deltas for user explicit preferences.
//
const userPrefs = {
  format: "format720p",
};

//
// Hmm. What should be the type of the parameter ‘userPrefs’?
//
function combinePrefs(
  defaultPrefs: UserPrefs,
  userPrefs: unknown,
): UserPrefs {
  return {
    ...defaultPrefs,
    //
    // Error: “Spread types may only be created from object types.”
    //
    ...userPrefs,
  };
}
