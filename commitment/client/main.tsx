import React from 'react';
import { createRoot } from 'react-dom/client';
import { Meteor } from 'meteor/meteor';
import { LandingPage } from '/imports/ui/LandingPage';
import '/imports/api/bookmarks';

Meteor.subscribe('bookmarks');

Meteor.startup(() => {
  const container = document.getElementById('react-target');
  const root = createRoot(container!);
  root.render(<LandingPage />);
});
