import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { LinksCollection, Link } from './links';

Meteor.methods({
    'links.insert'(title: string, url: string) {
    check(title, String);
    check(url, String);

    if (!url.startsWith('http')) {
        throw new Meteor.Error('invalid-url', 'URL must be valid and start with http or https');
    }

    const newLink: Link = {
        title,
        url,
        createdAt: new Date(),
    };

    return LinksCollection.insertAsync(newLink);
    },
    
    'links.remove'(linkId: string) {
        check(linkId, String);
        const link = LinksCollection.findOneAsync(linkId);
        if (!link) {
            throw new Meteor.Error('link-not-found', 'Link not found');
        }
        LinksCollection.removeAsync(linkId);
    },

    'links.isBookmarked'(url: string) {
        check(url, String);
        const link = LinksCollection.findOneAsync({ url });
        return !!link;  
    },
});