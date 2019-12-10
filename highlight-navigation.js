/**
 * My attempt at making a navigation menu highlighter in VanillaJS :D
 *
 * This script _does not_ generate the navigation menu. That is done by
 * Asciidoctor (in this project). The script just highlights the navigation
 * item that matches the currently visible heading in th main content
 * area of the document.
 *
 * Inspired by these projects:
 *
 * JavaScript Garden
 * -----------------
 *
 *   http://bonsaiden.github.io/JavaScript-Garden/
 *
 * Tocify, used in Anki Docs
 * --------------------------
 *
 *   http://gregfranko.com/jquery.tocify.js/
 *   https://apps.ankiweb.net/docs/manual.html
 *
 */


(function () {

  const l = console.log.bind(console);

  /**
  * Limits the amount of times a function is invoked.
  *
  * @param {function} - the function to be throttled.
  @ @param {number} wait - in milliseconds.
  */
  const throttle = function throttle (fn, wait = 0) {
    let time = Date.now();
    return function throttler () {
      if ((time + wait - Date.now() < 0)) {
        fn  ();
        time = Date.now();
      }
    }
  };

  const classAdd = function classAdd (elems, klass = 'active') {
    elems.forEach((elem) => {
      elem.classList.add(klass);
    });

    return elems;
  };

  const classRemove = function classRemove (elems, klass = 'active') {
    elems.forEach((elem) => {
      elem.classList.remove(klass);
    });

    return elems;
  };

  // Get all navigation <a> elements.
  const anchors = [...document.querySelectorAll('#header .toc2 a')];

  // Get all headings on the content area.
  const headings = [...document.querySelectorAll('#content h2, #content h3, #content h4')];

  const makeActive = function makeActive () {
    const { scrollX, innerHeight } = window;
    const { scrollTop } = document.documentElement;
    const scrollBottom = (window.innerHeight + window.pageYOffset) >= document.body.offsetHeight;

    headings.forEach((heading) => {
      const { top, bottom } = heading.getBoundingClientRect();

      if (scrollTop === 0) {
        classRemove(anchors);
        classAdd([anchors[0]]);
        return;
      }

      if (scrollBottom) {
        classRemove(anchors);
        classAdd([anchors[anchors.length - 1]]);
        return;
      }

      // l(parseInt(top), parseInt(bottom), parseInt(innerHeight / 2));
      if (top >= 0 && bottom <= innerHeight) {
        l(parseInt(top), parseInt(bottom), parseInt(innerHeight / 2));
        for (let i = 0; i < anchors.length; ++i) {
          const { href } = anchors[i];
          if (href.includes(heading.getAttribute('id'))) {
            l('=== matches', heading);
            classRemove(anchors)
            classAdd([anchors[i]])
            break;
          }
        }
      }
    });
  };

  (function boot () {
    let timerId = null;

    window.addEventListener('scroll', function () {
      if (timerId !== null) clearTimeout(timerId);
      timerId = setTimeout(makeActive, 50);
    }, false);

    // Run once initally.
    makeActive();
  })();

})();
