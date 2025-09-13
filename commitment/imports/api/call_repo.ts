import { Meteor } from 'meteor/meteor'
import { Mongo } from 'meteor/mongo'
import { Tracker } from 'meteor/tracker'
import { Subject } from "rxjs"
import { meteorCallAsync } from './meteor_interface';

// Define the schema for documents in the collection
interface PersonalServerResponse {
  _id?: string;
  text: string;
  createdAt?: Date;
}

const ServerResponses = new Mongo.Collection<PersonalServerResponse>("fetchRepoMessagesCollection");

export const fetchRepo = (url: string, subject: Subject<string>): Promise<boolean> => {
  // Subscribe to your personal message stream
  Meteor.subscribe('fetchRepoMessages')

  // Reactively log messages
  Tracker.autorun(() => {
    const messages: PersonalServerResponse[] = ServerResponses.find({}, { sort: { createdAt: -1 } }).fetch()
    messages.forEach((m: PersonalServerResponse) => subject.next(m.text));
  })

  return meteorCallAsync("getGitHubRepoData")(url)
}

export const repoInDatabase = (url: string): Promise<boolean> => {
  // Only check connection status on the client side
  if (Meteor.isClient && !Meteor.status().connected) {
    throw new Error("Server is not connected")
  }

  return meteorCallAsync("repoInDatabase")(url)
}


export const getMetric = <T>(url: string, f: string) => 
  meteorCallAsync<T>("getMetricFromRepo")(url, f)
