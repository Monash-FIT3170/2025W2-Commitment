import { Meteor } from 'meteor/meteor';
import { ServiceConfiguration } from 'meteor/service-configuration';

Meteor.startup(async () => {
  // Configure Google OAuth
    await ServiceConfiguration.configurations.upsertAsync(
    { service: 'google' },
    {
        $set: {
            clientId: process.env.GOOGLE_CLIENT_ID || Meteor.settings.google?.clientId,
            secret: process.env.GOOGLE_CLIENT_SECRET || Meteor.settings.google?.secret,
            loginStyle: 'popup',
        },
    }
    );

  // Configure GitHub OAuth
    await ServiceConfiguration.configurations.upsertAsync(
    { service: 'github' },
    {
        $set: {
            clientId: process.env.GITHUB_CLIENT_ID || Meteor.settings.github?.clientId,
            secret: process.env.GITHUB_CLIENT_SECRET || Meteor.settings.github?.secret,
            loginStyle: 'popup',
        },
    }
    );

});
