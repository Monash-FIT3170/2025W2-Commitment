import React, { useState } from 'react';
import {Button} from "/imports/ui/components/ui/button";

export const Hello = () => {
  const [counter, setCounter] = useState(0);

  const increment = () => {
    setCounter(counter + 1);
  };

  return (
    <div>
      <Button onClick={increment}>Click Me</Button>
      <p>You've pressed the button {counter} times.</p>
    </div>
  );
};
