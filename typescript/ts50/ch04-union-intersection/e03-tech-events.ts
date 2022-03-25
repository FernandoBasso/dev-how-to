export const NAME = "e03 tech events";

const log: Console["log"] = console.log.bind(console);

//
// The ‘&’ means we are making an intersection between
// ‘TechEventBase’ and the literal type on the right.
//
// The ‘|’ means we are creating a union type.
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
}

//
// Let's use intersection types to combine the base
// type with the few other properties that change
// from type to type of event.
//

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
};

//
// Pretty much the same as ‘Conference’, except that ‘price’ is
// a string ("free") instead of a number.
//
type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
};

//
// ‘Webinar’ has only one talk, and a URL instead of a
// physical location. ‘price’ is optional.
//
type Webinar = TechEventBase & {
  url: string;
  price?: string;
  talks: Talk; // Why plural in the book?
};

type TechEvent = Conference | Meetup | Webinar;


function printEvent(event: TechEvent) {
  if (event.price) {
    //
    // Price exists!
    //
    if (typeof event.price === 'number') {
      //
      // We know that price is a number
      //
      console.log('Price in EUR: ', event.price)
    } else {
      //
      // We know that price is a string, so the
      // event is free!
      //
      console.log('It is free!')
    }
  }
  if (Array.isArray(event.talks)) {
    //
    // talks is an array
    //
    event.talks.forEach(talk => {
      console.log(talk.title)
    })
  } else {
    //
    // It's just a single talk
    //
    console.log(event.talks.title)
  }
}
