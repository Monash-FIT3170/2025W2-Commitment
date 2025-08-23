import React from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Meteor } from 'meteor/meteor';
import { useTracker } from 'meteor/react-meteor-data';

export const SettingsPage: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;

  return (
    <div className="min-h-screen bg-git-bg-primary">
      <NavBar isLoggedIn={isLoggedIn} />
      
      <div className="max-w-6xl mx-auto px-6 py-8 pt-24">
        <h1 className="text-4xl font-bold text-git-text-primary mb-4">
          Settings page coming soon :D
        </h1>
      </div>
    </div>
  );
};

export default SettingsPage;
