import React from 'react';
import { Bookmark, ChevronRight } from "lucide-react";
import { Button } from "../ui/button";

interface RepoCardProps {
  repoName: string;
  repoDescription: string;
}

const RepoCard: React.FC<RepoCardProps> = ({ repoName, repoDescription }) => {
  return (
    <div className="border border-gray-300 rounded-md p-4 flex flex-col justify-between w-full max-w-xs">
      <div className="flex justify-between items-start">
        <div>
          <p className="text-sm text-gray-800 mb-2">{repoName}</p>
          <p className="text-xs text-gray-600 italic">{repoDescription}</p>
          <p className="text-xs text-gray-600 italic">Commit activity summary...</p>
          <p className="text-xs text-gray-600 italic">Last updated: date</p>
        </div>
        <Bookmark className="h-5 w-5 text-gray-500" />
      </div>
      <div className="flex justify-end mt-4">
        <Button variant="ghost" className="flex items-center text-orange-500">
          View <ChevronRight className="ml-1 h-4 w-4" />
        </Button>
      </div>
    </div>
  );
};

export default RepoCard; 