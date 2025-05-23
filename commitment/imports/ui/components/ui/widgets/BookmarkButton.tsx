import { Subject } from "rxjs"

import React, { useEffect, useState } from 'react';
import { Button } from '@ui/components/ui/button'; 
import { Meteor } from 'meteor/meteor';
import { Bookmark } from 'lucide-react';
import { fetchRepo, repoInDatabase } from '/client/call_repo';

type BookmarkRepoButtonProps = {
    url: string;
    title: string;
};

const BookmarkRepoButton: React.FC<BookmarkRepoButtonProps> = ({ url, title }) => {
    const [isBookmarked, setIsBookmarked] = useState(false);
    const [loading, setLoading] = useState(true);

    // Check if already bookmarked
    useEffect(() => {
        Meteor.call('links.isBookmarked', url, (err: any, result: boolean) => {
            if (!err) {
                setIsBookmarked(result);
            }
            setLoading(false);
        });
    }, [url]);

<<<<<<< HEAD
    // Adds the bookmark via Meteor method
    const handleAddBookmark = async () => {
        // Input validation
=======
    const handleClick = () => {
>>>>>>> parent of 475240b (feat(bookmark): integrate shadcn UI components and add BookmarkButton test)
        if (!url || !title) {
            alert('Missing URL or title');
            return;
        }

<<<<<<< HEAD
        // checks if a repo data struct is already in the database (not links collection)
        // if not, prompt the server to cache it in using the API server calls.
        if (!repoInDatabase(url)) {

            const updateNotifier = new Subject<string>()
            const repoCached = await fetchRepo(url, updateNotifier)

            if (!repoCached) {
                toast({
                    title: 'Error',
                    description: 'Failed to load url into database as it did not already exist',
                    variant: 'destructive',
                });
                return;
            }
        }

        Meteor.call('links.insert', title, url, (err: any) => {
            if (err) {
                toast({
                    title: 'Error saving repository',
                    description: err.reason,
                    variant: 'destructive',
=======
        if (isBookmarked) {
            const confirmUnbookmark = confirm('Do you want to remove this bookmark?');
            if (confirmUnbookmark) {
                Meteor.call('links.remove', url, (err: any) => {
                    if (err) {
                        alert(`Failed to remove bookmark: ${err.reason}`);
                    } else {
                        alert('Bookmark removed!');
                        setIsBookmarked(false);
                    }
>>>>>>> parent of 475240b (feat(bookmark): integrate shadcn UI components and add BookmarkButton test)
                });
            }
        } else {
            Meteor.call('links.insert', title, url, (err: any) => {
                if (err) {
                    alert(`Failed to save repo: ${err.reason}`);
                } else {
                    alert('Repository saved!');
                    setIsBookmarked(true);
                }
            });
        }
    };

    return (
        <Button
            variant={isBookmarked ? 'secondary' : 'default'}
            size="default"
            onClick={handleClick}
            disabled={loading}
            className="flex items-center gap-2"
        >
            <Bookmark
                className="w-4 h-4"
                fill={isBookmarked ? 'currentColor' : 'none'}
                stroke="currentColor"
            />
            {isBookmarked ? 'Bookmarked' : 'Bookmark'}
        </Button>
    );
};

export default BookmarkRepoButton;
