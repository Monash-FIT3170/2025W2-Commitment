import React from 'react';
import Purpose from '@ui/components/landing-page/PurposePage';
import MainPage from '@ui/components/landing-page/MainPage';

export const LandingPage = () => (
  <div className="m-9">
   
    <div className="flex flex-col gap-16">
        {/* Put components here */}
        <MainPage />
        <Purpose />
    </div>    
  </div>
);


