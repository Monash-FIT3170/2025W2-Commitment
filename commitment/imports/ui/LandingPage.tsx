import React from 'react';
import Purpose from '/imports/ui/components/landing-page/purpose';
import MainPage from './MainPage';

export const LandingPage = () => (
  <div className="m-9">
   
    <div className="flex flex-col gap-6">
        {/* Put components here */}
        <MainPage />
        <Purpose />
    </div>    
  </div>
);


