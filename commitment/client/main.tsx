import React from 'react';
import { createRoot } from 'react-dom/client';
import { Meteor } from 'meteor/meteor';
import { LandingPage } from '/imports/ui/LandingPage';
import {OverviewPage} from '../imports/ui/components/metrics-page/OverviewPage';
import { MetricsPage } from '../imports/ui/components/metrics-page/MetricsPage';
import { TestPage } from '/imports/ui/Test';

Meteor.startup(() => {
  const container = document.getElementById('react-target');
  const root = createRoot(container!);
  root.render(<TestPage />);
});
