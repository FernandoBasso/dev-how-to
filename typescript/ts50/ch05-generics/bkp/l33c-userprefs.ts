export const NAME = "l33c Mapped Type Modifiers";

//
// Partial<Type>
//

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
// Since we only store deltas for explicit user prefs, let's
// make an “all optional” version of ‘UserPrefs’.
//
function combinePrefs(
  defaultPrefs: UserPrefs,
  userPrefs: Partial<UserPrefs>,
): UserPrefs {
  return {
    ...defaultPrefs,
    //
    // Error: “Spread types may only be created from object types.”
    //
    ...userPrefs,
  };
}

//
// ‘Partial’ utility type could be defined like this:
//
type Optional<Obj> = {
  [Key in keyof Obj]?: Obj[Key];
};

const aaylaSecuraPrefs: Optional<UserPrefs> = {
  theme: "light",
};

const aaylaPrefs: UserPrefs =
  combinePrefs(defaultUserPrefs, aaylaSecuraPrefs);

const myPrefs: UsePrefs = combinePrefs(
  defaultUserPrefs,
  { format: "format240p" },
);

const otherPrefs: UserPrefs = combinePrefs(
  defaultUserPrefs,
  { foo: "bar" },
);

const yodaPrefs: UserPrefs = combinePrefs(
  defaultUserPrefs,
  { theme: "green" },
);
