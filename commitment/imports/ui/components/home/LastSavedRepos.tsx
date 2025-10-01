import { Meteor } from "meteor/meteor";
import React from "react";
import { useTracker } from "meteor/react-meteor-data";
import { ChevronRight } from "lucide-react";
import { Link } from "react-router-dom";
import RepoCard from "./RepoCard";
import { Button } from "@base/button";
import { Bookmark, BookmarksCollection } from "@api/bookmarks";
import { useNavigate } from "react-router-dom";

function LastSavedRepos() {
  // Use reactive data from the bookmarks publication (not repositories)
  const bookmarks = useTracker(() => {
    const subscription = Meteor.subscribe("bookmarks");
    if (subscription.ready()) {
      return BookmarksCollection.find(
        {},
        {
          sort: { createdAt: -1 },
          limit: 4,
        }
      ).fetch();
    }
    return [];
  }, []);

  const navigate = useNavigate();

  const handleViewRepository = (bookmark: Bookmark) => {
    // Update lastViewed timestamp
    Meteor.call("bookmarks.updateLastViewed", bookmark.url, (error: any) => {
      if (error) {
        console.error("Error updating lastViewed:", error);
      }
    });

    navigate("/metrics", {
      state: { repoUrl: bookmark.url },
    });
  };

  return (
    <div className="w-full max-w-4xl mt-8 p-6 bg-git-bg-secondary/20 dark:bg-git-bg-secondary border border-border rounded-lg">
      <h2 className="text-2xl font-semibold text-foreground mb-6 text-center">
        Last Saved Repositories
      </h2>

      {bookmarks.length === 0 ? (
        <div className="text-center text-gray-600 py-8">
          <p>No repositories have been bookmarked yet.</p>
          <p className="text-sm mt-2">
            Analyze a repository and bookmark it to get started!
          </p>
        </div>
      ) : (
        <>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-6">
            {bookmarks.map((bookmark) => (
              <RepoCard
                key={bookmark._id}
                repository={{
                  _id: bookmark._id,
                  url: bookmark.url,
                  name: bookmark.title,
                  owner: "Repository",
                  description: bookmark.title,
                  userID: bookmark.userID,
                  createdAt: bookmark.createdAt,
                  updatedAt: bookmark.createdAt,
                  analysisStatus: "completed",
                }}
                onClick={() => handleViewRepository(bookmark)}
              />
            ))}
          </div>

          <div className="flex justify-end mt-6">
            <Button variant="secondary">
              <Link to="/dashboard">See All</Link>{" "}
              <ChevronRight className="ml-1 h-4 w-4" />
            </Button>
          </div>
        </>
      )}
    </div>
  );
}

export default LastSavedRepos;
