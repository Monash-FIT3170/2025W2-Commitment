import React from 'react';
import { createRoot } from 'react-dom/client';
import { Meteor } from 'meteor/meteor';
import { LandingPage } from '/imports/ui/LandingPage';

Meteor.startup(() => {
  const container = document.getElementById('react-target');
  const root = createRoot(container!);
  root.render(<LandingPage />);
});
