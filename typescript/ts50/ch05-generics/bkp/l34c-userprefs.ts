export const NAME = "l34c Binding Generics";

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

type DefaultUserPrefs = {
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

const defaultUserPrefs: Const<DefaultUserPrefs> = {
  format: "format1080p",
  subtitles: {
    active: false,
    language: "english",
  },
  theme: "light",
};

//
// By using both generics, we substitute for type values
// upon invocation, and we end up with intersections that
// have not values in common, therefore producing ‘never’
// for both format and theme.
//
function combinePrefs<
  Defaults extends DefaultUserPrefs,
  UserPref extends Partial<DefaultUserPrefs>,
>(
  defaultPrefs: Defaults,
  userPrefs: UserPref,
): DefaultUserPrefs {
  return {
    ...defaultPrefs,
    ...userPrefs,
  };
}

const prefs = combinePrefs(
  defaultUserPrefs,
  { format: "format1080p", theme: "dark" },
);
//
// prefs.theme.<Tab> --> no light or dark. Only
// string methods...
//
