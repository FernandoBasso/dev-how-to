var log = console.log.bind(console);

var cntChild = 0;

function Child() {
  log('<Child /> render', ++cntChild);

  return <p>Child Component!</p>
}

var cntApp = 0;

export function App() {
  log('<App /> render', ++cntApp);

  return (
    <div className='app'>
      <h1>ex01</h1>

      <Child />
    </div>
  );
}
