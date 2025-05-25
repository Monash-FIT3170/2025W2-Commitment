import { Subject } from "rxjs"

import React, { useEffect, useState } from 'react';
import { Button } from '@ui/components/ui/button';
import {
    AlertDialog,
    AlertDialogTrigger,
    AlertDialogContent,
    AlertDialogHeader,
    AlertDialogTitle,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogCancel,
    AlertDialogAction,
} from '@ui/components/ui/alert-dialog';
import { useToast } from '@ui/hooks/use-toast';
import { Meteor } from 'meteor/meteor';
import { Bookmark } from 'lucide-react';
// Update the import path to the correct relative location of call_repo
import { fetchRepo, repoInDatabase } from '../../../api/call_repo';

/**
 * BookmarkRepoButton Component
 *
 * This button component allows users to bookmark or unbookmark a repository URL.
 * It checks whether the URL is already bookmarked and renders either an "add" or
 * "remove" action accordingly.
 *
 * Props:
 * - url (string): The repository URL to bookmark or unbookmark.
 * - title (string): The display name/title of the repository.
 *
 * Functionality:
 * - If the repo is already bookmarked, displays a confirmation dialog to remove it.
 * - If not bookmarked, clicking the button adds it to the bookmarks.
 * - Uses Meteor methods `links.isBookmarked`, `links.insert`, and `links.remove`.
 * - Displays toast notifications for success and error feedback.
 */
type BookmarkButtonProps = {
    url: string;
    title: string;
    currentUserID: Meteor.User | null; // Current user, can be null if not logged in
};

const BookmarkButton: React.FC<BookmarkButtonProps> = ({ url, title, currentUserID }) => {
    // Track whether the repo is already bookmarked
    const [isBookmarked, setIsBookmarked] = useState(false);

    // Track loading state while checking bookmark status
    const [loading, setLoading] = useState(true);

    // Controls visibility of the confirmation dialog
    const [showDialog, setShowDialog] = useState(false);

    // Toast hook for displaying success/error messages
    const { toast } = useToast();

    // On mount or when URL changes, check if it's already bookmarked
    useEffect(() => {
        Meteor.call('bookmarks.isBookmarked', url, currentUserID, (err: any, result: boolean) => {
            if (!err) {
                setIsBookmarked(result);
            }
            setLoading(false);
        });
    }, [url]);

    // Adds the bookmark via Meteor method
    const handleAddBookmark = async () => {
        // Input validation
        if (!url || !title) {
            toast({
                title: 'Error',
                description: 'Missing URL or title',
                variant: 'destructive',
            });
            return;
        }

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

        Meteor.call('bookmarks.insertBookmark', title, url, currentUserID, (err: any) => {
            if (err) {
                toast({
                    title: 'Error saving repository',
                    description: err.reason,
                    variant: 'destructive',
                });
            } else {
                toast({
                    title: 'Repository saved!',
                    description: `${title} has been bookmarked.`,
                });
                setIsBookmarked(true);
            }
        });
    };

    // Removes the bookmark via Meteor method
    const handleRemoveBookmark = () => {
        Meteor.call('bookmarks.removeBookmark', url, currentUserID, (err: any) => {
            if (err) {
                toast({
                    title: 'Error removing bookmark',
                    description: err.reason,
                    variant: 'destructive',
                });
            } else {
                toast({
                    title: 'Bookmark removed!',
                    description: `${title} has been unbookmarked.`,
                });
                setIsBookmarked(false);
            }
        });

        // Close the confirmation dialog
        setShowDialog(false);
    };

    return (
        <>
            {isBookmarked ? (
                // If already bookmarked, show a button that opens a confirmation dialog
                <AlertDialog open={showDialog} onOpenChange={setShowDialog}>
                    <AlertDialogTrigger asChild>
                        <Button
                            variant="secondary"
                            size="default"
                            disabled={loading}
                            className="w-10 h-10 p-0 flex items-center justify-center"
                        >
                            <Bookmark className="w-4 h-4" fill="currentColor" stroke="currentColor" />
                        </Button>
                    </AlertDialogTrigger>
                    <AlertDialogContent>
                        <AlertDialogHeader>
                            <AlertDialogTitle>Remove Bookmark?</AlertDialogTitle>
                            <AlertDialogDescription>
                                Are you sure you want to remove this repository from your bookmarks?
                            </AlertDialogDescription>
                        </AlertDialogHeader>
                        <AlertDialogFooter>
                            <AlertDialogCancel>Cancel</AlertDialogCancel>
                            <AlertDialogAction onClick={handleRemoveBookmark}>Remove</AlertDialogAction>
                        </AlertDialogFooter>
                    </AlertDialogContent>
                </AlertDialog>
            ) : (
                // If not bookmarked, show a button to add it
                <Button
                    variant="default"
                    size="default"
                    onClick={handleAddBookmark}
                    disabled={loading}
                    className="w-10 h-10 p-0 flex items-center justify-center"
                >
                    <Bookmark className="w-4 h-4" fill="none" stroke="currentColor" />
                </Button>
            )}
        </>
    );
};

export default BookmarkButton;