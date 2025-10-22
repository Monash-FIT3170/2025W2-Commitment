import { Meteor } from "meteor/meteor";
import { Mongo } from "meteor/mongo";
import { Tracker } from "meteor/tracker";
import { Subject } from "rxjs";
import { meteorCallAsync, emitValue } from "./meteor_interface";

// Define the schema for documents in the collection
interface PersonalServerResponse {
  _id?: string;
  text: string;
  createdAt?: Date;
}

const ServerResponses = new Mongo.Collection<PersonalServerResponse>("fetchRepoMessagesCollection");

export const fetchRepo = (
  url: string,
  subject: Subject<string> | null,
  queryDatabase: boolean = true
): Promise<boolean> => {
  // Subscribe to your personal message stream
  Meteor.subscribe("fetchRepoMessages");

  // Reactively log messages
  Tracker.autorun(() => {
    const messages: PersonalServerResponse[] = ServerResponses.find(
      {},
      { sort: { createdAt: -1 } }
    ).fetch();
    subject ? messages.forEach((m: PersonalServerResponse) => subject.next(m.text)) : null;
  });
  return meteorCallAsync("getGitHubRepoData")(url, queryDatabase);
};

export const repoInDatabase = (url: string): Promise<boolean> =>
  meteorCallAsync("repoInDatabase")(url);

export const getMetric = <T>(url: string, f: string) =>
  meteorCallAsync<T>("getMetricFromRepo")(url, f);

export const updateRepo = async (
  url: string,
  notifier: Subject<boolean>,
  msgs: Subject<string> | null
): Promise<boolean> => {
  const upToDate = await meteorCallAsync<boolean>("repoCollection.isUpToDate")(url);
  notifier.next(upToDate);
  if (!upToDate) {
    const ret = await fetchRepo(url, msgs, false);
    emitValue(msgs)("Data is ready for viewing!");
    return ret;
  }
  return false;
};
