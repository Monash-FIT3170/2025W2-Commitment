import { Meteor } from 'meteor/meteor'
import { Mongo } from 'meteor/mongo'
import { Tracker } from 'meteor/tracker'
import { Subject } from "rxjs"

const ServerResponses = new Mongo.Collection('fetchRepoMessagesCollection');

export const fetchRepo = (url: string, subject: Subject<string>) => {

    // Subscribe to your personal message stream
    Meteor.subscribe('fetchRepoMessages')

    // Reactively log messages
    Tracker.autorun(() => {
        const messages = ServerResponses.find({}, { sort: { createdAt: -1 } }).fetch()
        messages.forEach((m: string) => subject.next(m));
    })

    // Call the server method to start data retrieval
    return new Promise((resolve, reject) => {
        Meteor.call('getGitHubRepoData', url, (err, result) => {
            if (err) reject(err)
            resolve(result)
        });
    })
}

