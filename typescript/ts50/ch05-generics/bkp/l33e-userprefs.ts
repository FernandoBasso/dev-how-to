export const NAME = "l33e Mapped Type Modifiers and DeepReadonly<Type>";

//
// DeepPartial<Type> and DeepReadonly<Type>
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
// Not allowed.
//
defaultUserPrefs.theme = "dark";

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

const aaylaSecuraPrefs: Partial<UserPrefs> = {
  theme: "light",
};

const aaylaPrefs: UserPrefs =
  combinePrefs(defaultUserPrefs, aaylaSecuraPrefs);

//
// Partial works only one level deep. It doesn't automatically
// do nested readonly.
//
// Let's implement something cool:
//
type DeepReadonly<ObjType> = {
  readonly [Key in keyof ObjType]: DeepReadonly<ObjType[Key]>;
};

function genFrozenDefaults(prefs: UserPrefs): DeepReadonly<UserPrefs> {
  return Object.freeze(prefs);
}

//
// Also cool.
//
type DeepPartial<ObjType> = {
  [Key in keyof ObjType]?: DeepPartial<ObjType[Key]>;
};

//
// TypeScript knows to stop the recursion if Obj[Key] returns
// a primitive or value type, or a union of primitive or value
// types.
//

