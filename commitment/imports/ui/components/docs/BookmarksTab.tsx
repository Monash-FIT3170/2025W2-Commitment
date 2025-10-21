import React from "react";
import { Bookmark, LayoutDashboard } from "lucide-react";

const BookmarksTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
        <div className="flex items-center gap-2 mb-4">
          <Bookmark className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">3.0 Bookmarks and the Dashboard</h2>
        </div>
        <p className="text-git-text-secondary mb-6">
          Learn how to bookmark repositories and manage them from your dashboard.
        </p>

        <div className="space-y-4 text-git-text-secondary">
          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2 flex items-center gap-2">
              <Bookmark className="h-5 w-5 text-git-accent-primary" />
              How to Bookmark
            </h3>
            <p className="ml-6">
              Save repositories to your bookmarks for quick access later. Learn where to find the bookmark button
              and how to manage your saved repositories.
            </p>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2 flex items-center gap-2">
              <Bookmark className="h-5 w-5 text-git-accent-primary" />
              How to Unbookmark
            </h3>
            <p className="ml-6">
              Remove repositories from your bookmarks when you no longer need quick access to them.
            </p>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2 flex items-center gap-2">
              <LayoutDashboard className="h-5 w-5 text-git-accent-primary" />
              Accessing Metrics via Bookmarks
            </h3>
            <p className="ml-6 mb-3">
              Navigate directly to repository metrics from your dashboard bookmarks.
            </p>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2">Filtering and Sorting</h3>
            <div className="ml-6">
              <p className="mb-2">Organize your bookmarked repositories:</p>
              <ul className="list-disc list-inside ml-4 space-y-1">
                <li>Filter by repository name, language, or last updated date</li>
                <li>Sort by various criteria (alphabetical, date added, activity)</li>
                <li>Search through your bookmarks</li>
                <li>Group bookmarks by categories or tags</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default BookmarksTab;
