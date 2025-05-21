import React from 'react';
import RepoCard from './RepoCard';
import { Button } from "../ui/button";
import { cn } from "@ui/lib/utils";
import { ChevronRight } from "lucide-react";

const mockRepos = [
  { id: 1, repoName: 'Repository One', repoDescription: 'Short description of repo one.' },
  { id: 2, repoName: 'Repository Two', repoDescription: 'Short description of repo two.' },
  { id: 3, repoName: 'Repository Three', repoDescription: 'Another short description.' },
  { id: 4, repoName: 'Repository Four', repoDescription: 'Yet another description.' },
];

const LastSavedRepos = () => {
  return (
    <div className="w-full max-w-4xl mt-8 p-6 bg-gray-100 border border-gray-300 rounded-lg">
      <h2 className="text-2xl font-semibold text-gray-800 mb-6 text-center">Last Saved Repositories</h2>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {mockRepos.slice(0, 2).map(repo => (
          <RepoCard 
            key={repo.id} 
            repoName={repo.repoName} 
            repoDescription={repo.repoDescription} 
          />
        ))}
      </div>
      <div className="flex justify-end mt-6">
        <Button className={cn(
          "h-auto text-sm rounded-full text-center bg-gray-200 hover:bg-gray-300 text-gray-800 px-4 py-2 shadow-sm"
        )}>
          See All <ChevronRight className="ml-1 h-4 w-4" />
        </Button>
      </div>
    </div>
  );
};

export default LastSavedRepos; 