import React, { useState } from 'react';
import {Card} from '@ui/components/ui/card';
import {CardContent} from "@ui/components/ui/card";
import { Button } from '@ui/components/ui/button';

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
