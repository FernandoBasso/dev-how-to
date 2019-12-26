/* eslint-disable no-unused-vars */
import {
  l,
  testsPassed,
} from './lib';

import expect from 'expect';
import React from 'react';
import ReactDOM from 'react-dom';

import { createStore } from 'redux';

const counter = (state = 0, action) => {
  const { type } = action;

  switch (type) {
    case 'INCR':
      return state + 1;
    case 'DECR':
      return state - 1;
    default:
      // The default case handles non-existing or unknown action types.
      return state;
  }
};

const store = createStore(counter);

export const Counter = ({ value }) => {
  return (
    <div>{ value }</div>
  );
};

// Render it once, even before the first click.
const render = () => {
  ReactDOM.render(
    <Counter value={ store.getState() } />,
    document.getElementById('root'),
  );
};

store.subscribe(render);
render();

document.addEventListener('click', () => {
  store.dispatch({ type: 'INCR' });
}, false);
