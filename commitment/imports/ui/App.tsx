import React from 'react';
import { Hello } from './Hello';
import { Info } from './Info';
import { Card, CardContent, CardHeader } from './components/ui/card';
import { Menu } from 'lucide-react';

export const App = () => (
  <div className="m-9">
    <h1 className="text-cyan-700 font-sans text-5xl m-9 text-center font-thin">Welcome to Meteor!</h1>
    <div className="flex flex-col gap-6">
      <Card  className='bg-[#F1502F]'>
      <CardHeader>
          title 
          <Menu />
        </CardHeader>
        <CardContent>
          hihi
        </CardContent>
    </Card>
    <Hello />
      <Info />
    </div>
  </div>
);
