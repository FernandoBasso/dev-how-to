export const NAME = "l34a Binding Generics";

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

//
// This is the same as built-in ‘Readonly<Type>’.
//
type Const<Type> = {
  readonly [Key in keyof Type]: Type[Key];
}

//
// Now we can't modify the default user prefs.
//
const defaultUserPrefs: Const<UserPrefs> = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

//
// We can prevent runtime modifications with ‘Object.freeze()’.
//
// Attempting to mutate an object returned by ‘genDefaults()’
// would cause errors in TypeScript and at runtime with JavaScript.
//
function genDefaults(prefs: UserPrefs): Readonly<UserPrefs> {
  return Object.freeze(prefs);
}

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

type DeepReadonly<ObjType> = {
  readonly [Key in keyof ObjType]: DeepReadonly<ObjType[Key]>;
};

type DeepPartial<ObjType> = {
  [Key in keyof ObjType]?: DeepPartial<ObjType[Key]>;
};

function genFrozenDefaults(prefs: UserPrefs): DeepReadonly<UserPrefs> {
  return Object.freeze(prefs);
}

//
// Works. The inference for the theme thing works and
// the type checker is happy with it because there is
// no way it could change. Only changing the source
// code would change it.
//
combinePrefs(
  defaultUserPrefs,
  { theme: "light" },
);

//
// But consider this next:
//
const yodaTheme = { theme: "light" };
//
// theme can be any string, not only 'light' (or 'dark').
//

const lukePrefs: UserPrefs = combinePrefs(
  defaultUserPrefs,
  yodaTheme,
)
//
// There is no guarantee that yodaTheme would not
// be changed between defining it and the moment it
// is used as the second argument of combinePrefs().
//
// But if guarantee it won't change, then we are OK.
//

const vaderTheme = { theme: "dark" } as const;

const vaderPrefs: UserPrefs = combinePrefs(
  defaultUserPrefs,
  vaderTheme,
);

//
// Type annotations do a type check the moment we assign
// a value.
//
const ahsokaPrefsOverrides: Partial<UserPrefs> = {
  theme: "light",
  format: "format1080p",
};

//
// Then it works too because we did not use inference
// for ahsokaPrefsOverrides, but did a (correct) type
// annotation instead.
//
const ahsokaPrefs: UserPrefs = combinePrefs(
  defaultUserPrefs,
  ahsokaPrefsOverrides,
);
