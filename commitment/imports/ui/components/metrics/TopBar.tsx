import React, {  useEffect , useState } from 'react';
import { Meteor } from 'meteor/meteor';
import { useLocation } from 'react-router-dom';
import { AnalyticsData, Metadata } from '/imports/api/types';
import { useAuth } from "@ui/hooks/useAuth";
import BookmarkButton from '../dashboard/BookmarkButton';
import { useAuth } from '@hook/useAuth';

/**
 * JANKY METHOD FOR NOW taken from chatgpt: Extracts the repository name from a Git URL
 * @param url Git repository URL
 * @returns repo name string
 */
function getRepoNameFromUrl(url: string): string {
  if (!url) return "Unknown Repo";

  // Remove trailing slash if it exists
  const cleanUrl = url.endsWith("/") ? url.slice(0, -1) : url;

  // Split by "/" and take the last part
  const parts = cleanUrl.split("/");
  const repoNameWithGit = parts[parts.length - 1];

  // Remove .git suffix if it exists
  return repoNameWithGit.replace(/\.git$/, "");
}

export default function TopBar() {
  // call meteor method to find the name
  const signedIn = useAuth()
  const location = useLocation();
  const repoUrl: string | null = location.state?.repoUrl ?? null;
  const [repoName, setRepoName] = useState<string>("Loading...");
  const isAuthorized = useAuth();

  useEffect(() => {
    if (!repoUrl) return;

    const fetchMetadata = async () => {
      try {
        const data = await Meteor.callAsync("repo.getMetadata", repoUrl);
        setRepoName(getRepoNameFromUrl(data.repoUrl));
      } catch (error) {
        console.error("Error fetching metadata:", error);
        setRepoName("Unknown Repository");
      }
    };

    fetchMetadata();
  }, [repoUrl]);

  return (
    <div className="flex items-center justify-between px-3 py-3 pr-5 border-b  border-git-stroke-primary/40 bg-git-bg-elevated">
      <div className="flex items-center gap-3">
        <h2 className="text-lg font-semibold text-foreground">{repoName}</h2>
        {/* Bookmark button (only shown to signed in users) */}
        {(repoUrl && signedIn) && (
          <BookmarkButton url={repoUrl} title={repoName} variant="secondary" />
        )}
      </div>
    </div>
  );
}
