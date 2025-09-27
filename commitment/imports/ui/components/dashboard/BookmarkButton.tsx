import { Subject } from "rxjs";

import React, { useEffect, useState } from "react";
import { Button } from "@base/button";
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
} from "@base/alert-dialog";
import { useToast } from "@hook/useToast";
import { Meteor } from "meteor/meteor";
import { Bookmark } from "lucide-react";
// Update the import path to the correct relative location of call_repo
import { fetchRepo, repoInDatabase } from "@api/call_repo";

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
  variant?: "primary" | "secondary";
};

const BookmarkButton: React.FC<BookmarkButtonProps> = ({
  url,
  title,
  variant = "primary",
}) => {
  // used to switch between orange and white variants
  const variantOptions = {
    primary: "text-[#FFFFFF] ",
    secondary: "text-[#F1502F] ",
  };
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
    Meteor.call("bookmarks.isBookmarked", url, (err: any, result: boolean) => {
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
        title: "Error",
        description: "Missing URL or title",
        variant: "destructive",
      });
      return;
    }

    // checks if a repo data struct is already in the database (not links collection)
    // if not, prompt the server to cache it in using the API server calls.
    if (!(await repoInDatabase(url))) {
      const updateNotifier = new Subject<string>();
      const repoCached = await fetchRepo(url, updateNotifier);

      if (!repoCached) {
        toast({
          title: "Error",
          description:
            "Failed to load url into database as it did not already exist",
          variant: "destructive",
        });
        return;
      }
    }

    Meteor.call("bookmarks.insertBookmark", title, url, (err: any) => {
      if (err) {
        toast({
          title: "Error saving repository",
          description: err.reason,
          variant: "destructive",
        });
      } else {
        toast({
          title: "Repository saved!",
          description: `${title} has been bookmarked.`,
        });
        setIsBookmarked(true);
      }
    });
  };

  // Removes the bookmark via Meteor method
  const handleRemoveBookmark = () => {
    Meteor.call("bookmarks.removeBookmark", url, (err: any) => {
      if (err) {
        toast({
          title: "Error removing bookmark",
          description: err.reason,
          variant: "destructive",
        });
      } else {
        toast({
          title: "Bookmark removed!",
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
              variant="ghost"
              size="icon"
              disabled={loading}
              className={`${variantOptions[variant]}`}
            >
              <Bookmark
           size={24} 
           fill="currentColor"
                stroke="currentColor"
              />
            </Button>
          </AlertDialogTrigger>
          <AlertDialogContent>
            <AlertDialogHeader>
              <AlertDialogTitle>Remove Bookmark?</AlertDialogTitle>
              <AlertDialogDescription>
                Are you sure you want to remove this repository from your
                bookmarks?
              </AlertDialogDescription>
            </AlertDialogHeader>
            <AlertDialogFooter>
              <AlertDialogCancel>Cancel</AlertDialogCancel>
              <AlertDialogAction onClick={handleRemoveBookmark}>
                Remove
              </AlertDialogAction>
            </AlertDialogFooter>
          </AlertDialogContent>
        </AlertDialog>
      ) : (
        // If not bookmarked, show a button to add it
        <Button
          variant="ghost"
          size="icon"
          onClick={handleAddBookmark}
          disabled={loading}
          className={`${variantOptions[variant]}`}
        >
          <Bookmark 
           size={24} 
          fill="none" 
          stroke="currentColor" />
        </Button>
      )}
    </>
  );
};

export default BookmarkButton;
