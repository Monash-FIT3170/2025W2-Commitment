import React from 'react';
import { Settings } from 'lucide-react';
import { Meteor } from 'meteor/meteor';
import {  useEffect } from 'react';
import { useLocation } from 'react-router-dom';
import { useState } from 'react';


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
  const location = useLocation();
  const repoUrl: string | null = location.state?.repoUrl ?? null;
  const [repoName, setRepoName] = useState<string>('Loading...');

  useEffect(() => {
    if (!repoUrl) return;

    const fetchMetadata = async () => {
      try {
        const data = await Meteor.callAsync('repo.getMetadata', repoUrl);
        setRepoName(getRepoNameFromUrl(data.repoUrl));
      } catch (error) {
        console.error('Error fetching metadata:', error);
        setRepoName('Unknown Repository');
      }
    };

    fetchMetadata();
  }, [repoUrl]);


  return (
    <div className="flex items-center justify-between px-10 py-3 border-b border-border bg-git-bg-elevated">
      <div className="flex items-center gap-3">
        <h2 className="text-lg font-semibold text-foreground">{repoName}</h2>
        {/* Bookmark icon placeholder */}
        <div className="w-6 h-6 bg-gray-300 rounded" />
        {' '}
      </div>
      <Settings className="w-5 h-5 text-git-stroke-secondary hover:text-git-stroke-primary cursor-pointer" />
    </div>
  );
}
