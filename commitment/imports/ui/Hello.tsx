import React, { useState } from 'react';
import {Card} from './components/ui/card';
import {Button} from "/imports/ui/components/ui/button";
import {CardContent} from "@ui/components/ui/card";

export const Hello = () => {
  const [counter, setCounter] = useState(0);

  const increment = () => {
    setCounter(counter + 1);
  };

  return (
    <Card>
      <CardContent className="pt-6 flex flex-row gap-3 content-center">
        <Button onClick={increment}>Click Me</Button>
        <div className="inline-flex items-center">
          You've pressed the button {counter} times.
        </div>
      </CardContent>
    </Card>
  );
};
