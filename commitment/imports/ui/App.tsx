import React from 'react';
import { Hello } from './Hello';
import { Info } from './Info';

export const App = () => (
  <div>
    <h1 className="text-cyan-700 font-sans text-5xl m-64">Welcome to Meteor!</h1>
    <Hello />
    <Info />
  </div>
);
