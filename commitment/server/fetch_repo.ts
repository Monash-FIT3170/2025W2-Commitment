import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import { WebSocket } from "ws";

import { RepositoryData } from "../imports/api/types";
import { cacheIntoDatabase, tryFromDatabase, isInDatabase } from "../server/caching";

const clientMessageStreams: Record<string, Subject<string>> = {};

Meteor.publish("fetchRepoMessages", function () {
  const connectionId = this.connection.id;

  // Create subject if not exists
  if (!clientMessageStreams[connectionId]) {
    clientMessageStreams[connectionId] = new Subject<string>();
  }

  // get subject
  const subject = clientMessageStreams[connectionId];

  // First add an empty initial document
  this.added("fetchRepoMessagesCollection", connectionId, {
    text: "",
    createdAt: new Date(),
  });

  // Send message manually when .next() is called
  const subscription = subject.subscribe((msg: string) =>
    this.changed("fetchRepoMessagesCollection", connectionId, {
      text: msg,
      createdAt: new Date(),
    })
  );

  // Cleanup on stop
  this.onStop(() => subscription.unsubscribe());

  // Notify client initial data is ready
  this.ready();
});

Meteor.methods({
  async getGitHubRepoData(repoUrl: string) {
    // gets the current connection id to identify the stream the updates should be sent to
    const connectionId = this.connection!.id;
    const sub = clientMessageStreams[connectionId];

    // ensures a not null value is returned and a valid subject is used in some capacity
    const subject = sub || new Subject<string>();

    // returns whether it was successful in caching to the database or not
    return await getRepoData(repoUrl.trim(), subject)
      .then((_) => true)
      .catch((_e) => false);
  },
});

Meteor.methods({
  async repoInDatabase(repoUrl: string) {
    return isInDatabase(repoUrl);
  },
});

/**
 * Fetches repository data from an external source.
 *
 * @param url URL of the repository to fetch data from.
 * @param notifier Subject to notify about the status of the fetch operation.
 *
 * @returns {Promise<RepositoryData>} A promise that resolves to the fetched repository data.
 * @throws {Error} If there is an error during the fetch operation.
 */
export const getRepoData = async (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> =>
  tryFromDatabase(url, notifier).catch((_e) =>
    fetchDataFromHaskellAppWS(url, notifier)
      .catch((_e) => {
        notifier.next(`Websockets failed, trying HTTP...`);
        return fetchDataFromHaskellAppHTTP(url);
      })
      .then((data: RepositoryData) => {
        notifier.next("Consolidating new data into database...");
        cacheIntoDatabase(url, data);
        return data;
      })
      .catch((e) => {
        notifier.next(`API fetch failed: ${e}`);
        throw e;
      })
  );

/**
 * Fetches the repository data structure from the Haskell API
 * Uses Websockets to recieve reactive messages from the app
 *
 * @param url url to run the API on
 * @param notifier a message sender, so that responsive messages can be sent from the API regarding errors and statuses
 * @returns Promise<RepositoryData>: a promise of the API completion
 */
const fetchDataFromHaskellAppWS = async (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) => {
    notifier.next("Connecting to the API...");
    const socket = new WebSocket("ws://haskell-api:8081");

    socket.onopen = () => {
      // Step 1: Send repo URL
      notifier.next("Connected to the API!");
      socket.send(
        JSON.stringify({
          url: url,
        })
      );
    };

    socket.onmessage = (event: any) => {
      // Step 2: Await response from haskell app
      const data = event.data;
      const parsed = JSON.parse(data);

      if (parsed.type === "text_update") notifier.next(parsed.data);
      else if (parsed.type === "value") resolve(parsed.data);
      else if (parsed.type === "error") reject(parsed.message);
    };

    socket.onclose = () => {};
  });

/**
 * Fetches the repository data structure from the Haskell API
 * Uses HTTP to recieve data from the API when the Websockets fail
 *
 * @param url url to run the API on
 * @returns Promise<RepositoryData>: a promise of the API completion
 */
const fetchDataFromHaskellAppHTTP = async (url: string): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) =>
    fetch("http://haskell-api:8081", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ url: url }),
    }).then((response) => {
      if (!response.ok) reject(`Haskell API returned status ${response.status}`);
      response.json().then((d) => resolve(d.data));
    })
  );
