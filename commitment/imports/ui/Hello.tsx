import React, { useState } from 'react';

function Hello() {
  const [counter, setCounter] = useState(0);

  const increment = () => {
    setCounter(counter + 1);
  };

  return (
    <div>
      <button type="button" onClick={increment}>
        Click Me
      </button>
      <p>
        You&apos;ve pressed the button
        {' '}
        {counter}
        {' '}
        times.
      </p>
    </div>
  );
}

export default Hello;
