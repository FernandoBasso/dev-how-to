export const NAME = "l34b Binding Generics";

const log: Console["log"] = console.log.bind(console);

type DeepReadonly<ObjType> = {
  readonly [Key in keyof ObjType]: DeepReadonly<ObjType[Key]>;
};

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

type UserPrefs = {
  format: keyof VideoFormatURLs;
  subtitles: {
    active: boolean;
    language: keyof SubtitleURLs;
  };
  theme: "light" | "dark";
};

type Const<Type> = {
  readonly [Key in keyof Type]: Type[Key];
}

const defaultUserPrefs: Const<UserPrefs> = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

function genDefaults(prefs: UserPrefs): Readonly<UserPrefs> {
  return Object.freeze(prefs);
}

function combinePrefs<
  UserPref extends Partial<UserPrefs>,
>(
  defaultPrefs: UserPrefs,
  userPrefs: UserPref,
): UserPrefs {
  return {
    ...defaultPrefs,
    ...userPrefs,
  };
}

combinePrefs(
  defaultUserPrefs,
  { theme: "light" },
)
