import { Meteor } from 'meteor/meteor'
import { Mongo } from 'meteor/mongo'
import { Tracker } from 'meteor/tracker'
import { Subject } from "rxjs"

const ServerResponses = new Mongo.Collection('fetchRepoMessagesCollection');

type PersonalMessageObject = Readonly<{ text: string }>

export const fetchRepo = (url: string, subject: Subject<string>): Promise<boolean> => 
    new Promise<boolean>((resolve, reject) => {
			// Subscribe to your personal message stream
			Meteor.subscribe('fetchRepoMessages')

			// Reactively log messages
			Tracker.autorun(() => {
				const messages: [PersonalMessageObject] = ServerResponses.find({}, { sort: { createdAt: -1 } }).fetch()
				messages.forEach((m: PersonalMessageObject) => subject.next(m.text));
			})

			// Call the server method to start data retrieval
			if (!Meteor.status().connected) {
				reject(new Error("Server is not found"))
			}

			Meteor.call('getGitHubRepoData', url, (err: Error, result: boolean) => {
				if (err) reject(err)
				resolve(result)
			});
})

export const repoInDatabase = async (url: string) => 
    new Promise<boolean>((resolve, reject) => {
        Meteor.call('repoInDatabase', url, (err: Error, result: boolean) => {
					if (err) reject(err)    
					resolve(result)
        });
    })

export const getMetric = async <T>(url: string, f: string) => 
    new Promise<T>((resolve, reject) => {
        Meteor.call('getMetricFromRepo', url, f, (err: Error, result: T) => {
            if (err) reject(err)    
            resolve(result)
        });
    })
