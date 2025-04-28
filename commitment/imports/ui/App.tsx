import React from 'react';
import { Hello } from './Hello';
import { Info } from './Info';

export const App = () => (
  <div className="m-9">
    <h1 className="text-cyan-700 font-sans text-5xl m-9 text-center font-thin">Welcome to Meteor!</h1>
    <div className="flex flex-col gap-6">
      <Hello />
      <Info />
    </div>
  </div>
);
