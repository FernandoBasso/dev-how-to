import {
  l,
} from './lib';

import expect from 'expect';

const counter = (state, action) => {
  const { type } = action;

  switch (type) {
    case 'INCR':
      return state + 1;
    case 'DECR':
      return state - 1;
    default:
      return state;
  }

};

expect(counter(0, { type: 'INCR' })).toEqual(1);
expect(counter(1, { type: 'DECR' })).toEqual(0);

l('=== All Tests Passed! ===');
