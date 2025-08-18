import React from 'react';
import { Hello } from '@ui/components/widgets/examples/Hello';
import { Info } from '@ui/components/widgets/examples/Info';
import { TestFilterSort } from '../../components/widgets/TestFilterSort';

function ExampleView() {
  return (
    <div className="m-9">
      <h1 className="text-cyan-700 font-sans text-5xl m-9 text-center font-thin">Welcome to Meteor!</h1>
      <div className="flex flex-col gap-6">
        <TestFilterSort />
        <Hello />
        <Info />
      </div>
    </div>
  );
}

export default ExampleView;
