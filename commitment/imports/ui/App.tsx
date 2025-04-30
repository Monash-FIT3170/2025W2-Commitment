import React from 'react';
import { Hello } from './Hello';
import { Info } from './Info';
import LoginWidget from './components/widgets/login/LoginWidget';

export const App = () => (
  <div className="m-9">
    <h1 className="text-primary font-sans text-5xl m-9 text-center font-thin">Welcome to Meteor!</h1>
    <div className="flex flex-col gap-6">
      <Hello />
      <Info />
      <LoginWidget/>
    </div>
  </div>
);
