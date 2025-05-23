import React from 'react';
import { createRoot } from 'react-dom/client';
import { Meteor } from 'meteor/meteor';
import { LandingPage } from '/imports/ui/LandingPage';
import {OverviewPage} from '/imports/ui/OverviewPage';
import { MetricsPage } from '/imports/ui/MetricsPage';

Meteor.startup(() => {
  const container = document.getElementById('react-target');
  const root = createRoot(container!);
  root.render(<MetricsPage />);
});
