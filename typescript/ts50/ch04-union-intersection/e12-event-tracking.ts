export const NAME = "e12 event tracking";

const log: Console["log"] = console.log.bind(console);

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

type Conference = TechEventBase & {
  kind: "conference";
  location: string;
  price: number;
  talks: Talk[];
};

type Meetup = TechEventBase & {
  kind: "meetup";
  location: string;
  price: string;
  talks: Talk[];
};

type Webinar = TechEventBase & {
  kind: "webinar";
  url: string;
  price?: string;
  talks: Talk; // Why plural in the book?
};

type Hackaton = TechEventBase & {
  kind: "hackathon";
  url: string;
  price?: number;
}

type TechEvent = Conference | Meetup | Webinar | Hackaton;

//
// <1>
//
// type GroupedEvents = {
//   conference: TechEvent[],
//   meetup: TechEvent[],
//   webinar: TechEvent[],
//   hackathon: TechEvent[]
// }

type EventKind = TechEvent["kind"];

type UserEvents = {
  watching: TechEvent[];
  rsvp: TechEvent[];
  attended: TechEvent[];
  signedout: TechEvent[];
};


