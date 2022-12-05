import { useState } from 'react';

var log = console.log.bind(console);

var cntChild = 0;

//
// Not receiving any props, but when the parent <App />
// state changes, <Child /> will re-render too!
//
function Child() {
  const [cnt, setCnt] = useState(0);
  log('<Child /> render', ++cntChild);

  return (
    <div>
      <p>Child Component!</p>
      <button onClick={() => setCnt(n => 1 + n)}>
        Child click!
      </button>
    </div>
  );
}

var cntApp = 0;

export function App() {
  const [count, setCount] = useState(0);
  log('<App /> render', ++cntApp);

  return (
    <div className='app'>
      <h1>ex01</h1>

      <Child />

      <button onClick={() => setCount(n => n + 1)}>
        Click me once!
      </button>
    </div>
  );
}
