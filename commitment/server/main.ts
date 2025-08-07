import { Meteor } from 'meteor/meteor';
import { Accounts } from 'meteor/accounts-base';
import { BookmarksCollection } from '../imports/api/bookmarks';

import '../imports/api/methods';

import "../imports/api/bookmark_methods";

import "./fetch_repo"
import "./caching"

Meteor.startup(async () => {
  const userCount = await Meteor.users.find().countAsync();
  const bookmarkCount = await BookmarksCollection.find().countAsync();

  if (userCount === 0) {
    // Create a few test users
    const users = [
      {
        email: 'admin@test.com',
        password: 'password123',
        profile: { name: 'Admin User' },
      },
      {
        email: 'user@test.com',
        password: 'password123',
        profile: { name: 'Test User' },
      },
      {
        email: 'demo@test.com',
        password: 'password123',
        profile: { name: 'Demo User' },
      },
    ];

    // test bookmarks
    const test_bookmark: Record<
      string,
      {
        title: string;
        url: string;
      }[]
    > = {
      'admin@test.com': [
        {
          title: 'Jellyfin (Admin repo)',
          url: 'https://github.com/jellyfin/jellyfin.git',
        },
        {
          title: 'LLM course (admin)',
          url: 'https://github.com/mlabonne/llm-coursehttps://github.com/mlabonne/llm-course.git',
        },
        {
          title: 'Qlib (Admin repo)',
          url: 'https://github.com/microsoft/qlib.git',
        },
      ],
      'user@test.com': [
        {
          title: 'Jellyfin (user repo)',
          url: 'https://github.com/jellyfin/jellyfin.git',
        },
        {
          title: 'LLM course (user)',
          url: 'https://github.com/mlabonne/llm-coursehttps://github.com/mlabonne/llm-course.git',
        },
        {
          title: 'Qlib (user repo)',
          url: 'https://github.com/microsoft/qlib.git',
        },
      ],
      'demo@test.com': [
        {
          title: 'Jellyfin (Demo repo)',
          url: 'https://github.com/jellyfin/jellyfin.git',
        },
        {
          title: 'LLM course (Demo)',
          url: 'https://github.com/mlabonne/llm-coursehttps://github.com/mlabonne/llm-course.git',
        },
        {
          title: 'Qlib (Demo repo)',
          url: 'https://github.com/microsoft/qlib.git',
        },
      ],
    };

    users.forEach((userData) => {
      const userId = Accounts.createUser({
        email: userData.email,
        password: userData.password,
        profile: userData.profile,
      });

      const bm = test_bookmark[userData.email];

      bm.forEach((bookmark) => {
        BookmarksCollection.insertAsync(
          {
            title: bookmark.title,
            url: bookmark.url,
            userID: userId,
            createdAt: new Date(),
          },
        );

        console.log(`Inserting bookmark for ${userData.email} (${userId}): ${bookmark.title}`);
      });

      console.log(`Created user: ${userData.email} with ID: ${userId}`);
    });
  } else if (bookmarkCount == 0) {
    const firstTwoUsers = await Meteor.users.find({}, { limit: 3 }).fetchAsync();
    console.log('adding bookmarks to :', firstTwoUsers);

    const testBookmarks = [
      {
        title: 'Jellyfin ',
        url: 'https://github.com/jellyfin/jellyfin.git',
        createdAt: new Date('2025-01-05T10:00:00Z'),
      },
      {
        title: 'LLM course ',
        url: 'https://github.com/mlabonne/llm-course.git',
        createdAt: new Date('2025-01-10T14:15:00Z'),
      },
      {
        title: 'Qlib',
        url: 'https://github.com/microsoft/qlib.git',
        createdAt: new Date('2025-01-12T09:30:00Z'),
      },
      {
        title: 'TypeScript Handbook Repo',
        url: 'https://github.com/microsoft/TypeScript-Handbook.git',
        createdAt: new Date('2025-02-01T08:00:00Z'),
      },
      {
        title: 'Node.js ',
        url: 'https://github.com/nodejs/node.git',
        createdAt: new Date('2025-02-14T12:45:00Z'),
      },
      {
        title: 'React Tutorial ',
        url: 'https://github.com/reactjs/react-tutorial.git',
        createdAt: new Date('2025-03-03T11:10:00Z'),
      },
      {
        title: 'MongoDB University Labs',
        url: 'https://github.com/mongodb-university/atlas-labs.git',
        createdAt: new Date('2025-03-22T16:30:00Z'),
      },
    ];

    if (firstTwoUsers[0]) {
      console.log(`Inserting into user ${firstTwoUsers[0].profile.name} with id ${firstTwoUsers[0]._id}`);
      testBookmarks.slice(0, 2).forEach((bookmark) => {
        BookmarksCollection.insertAsync({
          title: bookmark.title,
          url: bookmark.url,
          userID: firstTwoUsers[0]._id,
          createdAt: bookmark.createdAt,
        });
      });
    }

    if (firstTwoUsers[1]) {
      console.log(`Inserting into user ${firstTwoUsers[1].profile.name} with id ${firstTwoUsers[1]._id}`);
      testBookmarks.slice(2, 5).forEach((bookmark) => {
        BookmarksCollection.insertAsync({
          title: bookmark.title,
          url: bookmark.url,
          userID: firstTwoUsers[1]._id,
          createdAt: bookmark.createdAt,
        });
      });
    }

    if (firstTwoUsers[2]) {
      console.log(`Inserting into user ${firstTwoUsers[2].profile.name} with id ${firstTwoUsers[2]._id}`);
      [testBookmarks[5]].forEach((bookmark) => {
        BookmarksCollection.insertAsync({
          title: bookmark.title,
          url: bookmark.url,
          userID: firstTwoUsers[2]._id,
          createdAt: bookmark.createdAt,
        });
      });
    }
  } else {
    // BookmarksCollection.removeAsync({});
    console.log('Test bookmarks and users aleady set');
  }

  // We publish the entire Links collection to all clients.
  // In order to be fetched in real-time to the clients
  Meteor.publish('bookmarks', function () {
    return BookmarksCollection.find();
  });
});
