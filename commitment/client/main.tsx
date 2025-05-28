import React from 'react';
import { createRoot } from 'react-dom/client';
import { Meteor } from 'meteor/meteor';
import '/imports/api/bookmarks';
import { App } from '/imports/ui/App';

Meteor.subscribe('bookmarks');

Meteor.startup(() => {
  const container = document.getElementById('react-target');
  const root = createRoot(container!);
  root.render(<App/>);
});
