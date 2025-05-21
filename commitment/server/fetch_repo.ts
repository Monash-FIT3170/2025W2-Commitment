import { Meteor } from 'meteor/meteor'
import { Subject } from "rxjs"

import { getRepoData } from "../server/caching"

const clientMessageStreams: Record<string, Subject<string>> = {}

Meteor.publish('fetchRepoMessages', function () {
    const connectionId = this.connection.id

    // Create subject if not exists
    if (!clientMessageStreams[connectionId]) {
        clientMessageStreams[connectionId] = new Subject<string>()
    }

    // get subject
    const subject = clientMessageStreams[connectionId]

    // Send message manually when .next() is called
    const subscription = subject.subscribe((msg: string) => self.changed(
        'fetchRepoMessagesCollection', 
        connectionId, 
        {
            text: msg,
            createdAt: new Date()
        }
    ))

    // Cleanup on stop
    this.onStop(() => subscription.unsubscribe())

    // Notify client initial data is ready
    this.ready()
})


Meteor.methods({
    async getGitHubRepoData(repoUrl: string) {
        // gets the current connection id to identify the stream the updates should be sent to
        const connectionId = this.connection.id
        const sub = clientMessageStreams[connectionId]

        // ensures a not null value is returned and a valid subject is used in some capacity
        const subject = sub ? sub : new Subject<string>()

        // returns whether it was successful in caching to the database or not
        try {
            await getRepoData(repoUrl, subject)
            return true 

        } catch (error) {
            return false
        }
    }
})
