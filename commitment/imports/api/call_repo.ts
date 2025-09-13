import { Meteor } from 'meteor/meteor'
import { Mongo } from 'meteor/mongo'
import { Tracker } from 'meteor/tracker'
import { Subject } from "rxjs"

// Define the schema for documents in the collection
interface PersonalServerResponse {
  _id?: string;
  text: string;
  createdAt?: Date;
}

const ServerResponses = new Mongo.Collection<PersonalServerResponse>("fetchRepoMessagesCollection");

export const fetchRepo = (url: string, subject: Subject<string>): Promise<boolean> => 
    new Promise<boolean>((resolve, reject) => {
			// Subscribe to your personal message stream
			Meteor.subscribe('fetchRepoMessages')

			// Reactively log messages
			Tracker.autorun(() => {
				const messages: PersonalServerResponse[] = ServerResponses.find({}, { sort: { createdAt: -1 } }).fetch()
				messages.forEach((m: PersonalServerResponse) => subject.next(m.text));
			})

			// Call the server method to start data retrieval
			if (Meteor.isClient && !Meteor.status().connected) {
				reject(new Error("Server is not found"))
			}

			Meteor.call('getGitHubRepoData', url, (err: Error, result: boolean) => {
				if (err) reject(err)
				resolve(result ?? false)
			});
		})

export const repoInDatabase = async (url: string): Promise<boolean> => new Promise<boolean>((resolve, reject) => {
        // Only check connection status on the client side
        if (Meteor.isClient && !Meteor.status().connected) {
            return reject(new Error("Server is not connected"));
        }

        Meteor.call('repoInDatabase', url, (err: Error, result: boolean) => {
            if (err) {
                console.error('Error checking if repo is in database:', err);
                reject(err);
            } else {
                resolve(result);
            }
        });
    })

export const getMetric = async <T>(url: string, f: string) => 
    new Promise<T>((resolve, reject) => {
        Meteor.call('getMetricFromRepo', url, f, (err: Error, result: T) => {
            if (err) reject(err)    
            resolve(result)
        });
    })
