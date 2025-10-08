import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import { WebSocket } from "ws";
import net from "net";
import dotenv from "dotenv";

import { RepositoryData, SerializableRepoData } from "@api/types";
import { serializeRepoData, assertRepoTyping } from "@api/serialisation";
import { emitValue } from "@api/meteor_interface";

import { cacheIntoDatabase, isInDatabase, tryFromDatabaseViaLatest } from "./caching";

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
  async getGitHubRepoData(repoUrl: string, queryDatabase: boolean = true) {
    // gets the current connection id to identify the stream the updates should be sent to
    const connectionId = this.connection!.id;
    const sub = clientMessageStreams[connectionId];

    // ensures a not null value is returned and a valid subject is used in some capacity
    const subject = sub || new Subject<string>();

    // returns whether it was successful in caching to the database or not
    return await getRepoData(repoUrl.trim(), subject, queryDatabase).then((_) => true);
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

type ApiFetchStruct = {
  promise: Promise<RepositoryData>;
  notifier: Subject<string>;
};

const apiFetchPromises: Record<string, ApiFetchStruct> = {};

/**
 * takes a function which can fetch from the API and allows it to run through a request caching system
 * @param f function to used when calling the cached pipeline
 * @returns struct of data from the API call by the function
 */
export const pipeRepoDataViaCache =
  (f: (url: string, notifier: Subject<string> | null) => Promise<RepositoryData>) =>
  async (url: string, notifier: Subject<string> | null): Promise<RepositoryData> => {
    // we know database fetch failed, so lets join all same url requests together
    // check entry in record
    const existingPromise = apiFetchPromises[url];
    if (existingPromise === null || existingPromise === undefined) {
      // put new promise into the record to allow other callers to await this request
      // this ensures that API requests are only made once per repo url
      const sub = new Subject<string>();
      const emitSub = emitValue(sub);
      const newEntry = {
        promise: f(url, sub)
          .then(assertRepoTyping)
          .then(async (d: RepositoryData) => {
            emitSub("Caching data into the database...");
            await cacheIntoDatabase(url, d);
            return d;
          })
          .catch((e2: Error) => {
            emitSub(`API fetch failed: ${e2}`);
            throw e2;
          }),
        notifier: sub,
      } as ApiFetchStruct;
      apiFetchPromises[url] = newEntry;
    }

    const entry = apiFetchPromises[url];
    notifier ? entry.notifier.subscribe(emitValue(notifier)) : null;
    const awaitedValue = await entry.promise;
    // we can safely delete it here from the record to ensure that any other promises will fetch fresh data
    // if it is out of date from the database :3
    delete apiFetchPromises[url];
    return awaitedValue;
  };

/**
 * Fetches repository data from the external source APi.
 *
 * @param url URL of the repository to fetch data from.
 * @param notifier Subject to notify about the status of the fetch operation.
 *
 * @returns {Promise<RepositoryData>} A promise that resolves to the fetched repository data.
 * @throws {Error} If there is an error during the fetch operation.
 */
export const getRepoData = (
  url: string,
  notifier: Subject<string> | null,
  queryDatabase: boolean = true
): Promise<RepositoryData> => {
  const getRepo = () => fetchRepoData(url, notifier);
  return queryDatabase
    ? tryFromDatabaseViaLatest(url, notifier).catch((_e: Error) => getRepo())
    : getRepo();
};

export const getSerialisedRepoData = (
  url: string,
  notifier: Subject<string> | null
): Promise<SerializableRepoData> => getRepoData(url, notifier).then(serializeRepoData);

const SOCKET_PATH = "/tmp/haskell-ipc.sock";
const MAX_PAYLOAD_SIZE_MB = 100;

/**
 * Fetches the repository data structure from the Haskell API
 * Using Unix IPC mechanisms to bypass the network layer
 *
 * @param url url to run the API on
 * @param notifier a message sender, so that responsive messages can be sent from the API regarding errors and statuses
 * @returns Promise<RepositoryData>: a promise of the API completion
 */
export const fetchDataFromHaskellAppIPC = (
  url: string,
  notifier: Subject<string> | null
): Promise<RepositoryData> =>
  fetchDataFromHaskellAppFromSocket(
    url,
    notifier,
    new WebSocket("ws://" + API_CONN_ENDPOINT, {
      socketPath: SOCKET_PATH,
      perMessageDeflate: false, // optional, disables compression
      maxPayload: MAX_PAYLOAD_SIZE_MB * 1024 * 1024, // optional, large messages support
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
export const fetchDataFromHaskellAppWS = (
  url: string,
  notifier: Subject<string> | null
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
  notifier: Subject<string> | null,
  socket: WebSocket
): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) => {
    const emit = emitValue(notifier);
    emit("Connecting to the API...");

    socket.onopen = () => {
      // notify that connection to the api was successful
      emit("Connected to the API!");
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

        if (parsed.type === "text_update") emit(parsed.data);
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
      emit(s);
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
export const fetchDataFromHaskellAppHTTP = (
  url: string,
  notifier: Subject<string> | null
): Promise<RepositoryData> =>
  new Promise<RepositoryData>((resolve, reject) => {
    emitValue(notifier)("Fetching from the API...");
    fetch("http://" + API_CONN_ENDPOINT, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ url }),
    }).then((response) => {
      const errMsg = `Haskell API returned status ${response.status}`;
      emitValue(notifier)(errMsg);
      if (!response.ok) reject(Error(errMsg));
      response.json().then((d) => resolve(d.data));
    });
  });

export const fetchRepoData = pipeRepoDataViaCache(fetchDataFromHaskellAppIPC);
