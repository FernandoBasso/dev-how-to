import React, { useState, useEffect } from 'react';

function Form() {
  const [render, setRender] = useState(false);

  useEffect(() => {
    if (render) return;

    const timeoutId = setTimeout(function handleTimeout() {
      setRender(true);
    }, 5812);

    return function cleanUp() {
      return clearTimeout(timeoutId);
    };
  }, [render]);

  if (!render) return null;

  return (
    <div className="field">
      <h2 id="promos-section">Promos</h2>
      <label htmlFor="promos">Promo Input:</label>
      <input id="promos"type="text" placeholder="Promo code" />
    </div>
  );
}

function App() {
  const ps = new Array(5).fill(
      'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'
  );

  return (
    <div className="app">
      {ps.map((t, i) => <p key={i}>{t}</p>)}

      <Form />

      {ps.map((t, i) => <p key={i}>{t}</p>)}
    </div>
  );
}

export { App };
