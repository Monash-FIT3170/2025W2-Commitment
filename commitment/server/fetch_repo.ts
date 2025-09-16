import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import { WebSocket } from "ws";
import net from "net";
import dotenv from "dotenv";

import { RepositoryData } from "../imports/api/types";
import { assertRepoTyping } from "../imports/api/serialisation";
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
    return await getRepoData(repoUrl.trim(), subject).then((_) => true);
  },
});

Meteor.methods({
  async repoInDatabase(repoUrl: string) {
    return isInDatabase(repoUrl);
  },
});

// can have a case here to see if it is deployment or a docker localhost
// this means that the API can be connected to without the connection being hard coded
// Load environment variables
dotenv.config();
const DEV_API_CONN_ENDPOINT = "haskell-api:8081";
const DEPLOYMENT_API_CONN_ENDPOINT = process.env.API_CONN_ENDPOINT; // "54.66.80.27:8081";
const API_CONN_ENDPOINT = DEPLOYMENT_API_CONN_ENDPOINT || DEV_API_CONN_ENDPOINT;

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
  tryFromDatabase(url, notifier).catch((_e1) =>
    fetchDataFromHaskellAppWS(url, notifier)
      .then(assertRepoTyping) // enforces strong typing for the entire data structure
      .then((data: RepositoryData) => {
        notifier.next("Consolidating new data into database...");
        cacheIntoDatabase(url, data);
        return data;
      })
      .catch((e2) => {
        notifier.next(`API fetch failed: ${e2}`);
        throw e2;
      })
  );

/**
 * Fetches the repository data structure from the Haskell API
 * Using Unix IPC mechanisms to bypass the network layer
 *
 * @param url url to run the API on
 * @param notifier a message sender, so that responsive messages can be sent from the API regarding errors and statuses
 * @returns Promise<RepositoryData>: a promise of the API completion
 */
const fetchDataFromHaskellAppIPC = async (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) => {
    const path = "/tmp/haskell-ipc.sock";

    // Connect to Unix socket
    const socket = net.createConnection(path, () => {
      const ws = new WebSocket(null);
      ws.setSocket(socket, null, 100 * 1024 * 1024); // hijack underlying socket (sets max msg size to 100MB)
      fetchDataFromHaskellAppFromSocket(url, notifier, ws)
        .then((d) => resolve(d))
        .catch((e) => reject(e));
    });
  });

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
  fetchDataFromHaskellAppFromSocket(url, notifier, new WebSocket("ws://" + API_CONN_ENDPOINT));

/**
 * Fetches the repository data structure from the Haskell API
 * Uses Websockets to recieve reactive messages from the app
 *
 * @param url url to run the API on
 * @param notifier a message sender, so that responsive messages can be sent from the API regarding errors and statuses
 * @param socket a WebSocket to send the messages over
 * @returns Promise<RepositoryData>: a promise of the API completion
 */
const fetchDataFromHaskellAppFromSocket = async (
  url: string,
  notifier: Subject<string>,
  socket: WebSocket
): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) => {
    notifier.next("Connecting to the API...");

    socket.onopen = () => {
      // notify that connection to the api was successful
      notifier.next("Connected to the API!");
      // send data through socket
      socket.send(
        JSON.stringify({
          url,
        })
      );
    };

    socket.onmessage = (event: WebSocket.MessageEvent) => {
      // Step 2: Await response from haskell app
      try {
        const { data } = event;
        const parsed = JSON.parse(data);

        if (parsed.type === "text_update") notifier.next(parsed.data);
        else if (parsed.type === "error") reject(parsed.message);
        else if (parsed.type === "value") {
          resolve(parsed.data);
          socket.close();
        }
      } catch (err) {
        reject(err);
        socket.close();
      }
    };

    socket.onerror = (_err: WebSocket.ErrorEvent) => {
      const s = "Encountered a Websocket Error";
      notifier.next(s);
      reject(new Error(s));
      socket.close();
    };
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
    fetch("http://" + API_CONN_ENDPOINT, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ url }),
    }).then((response) => {
      if (!response.ok) reject(`Haskell API returned status ${response.status}`);
      response.json().then((d) => resolve(d.data));
    })
  );
