import { Meteor } from 'meteor/meteor';
import React, { useEffect, useState } from 'react';
import { cn } from '@ui/lib/utils';
import { ChevronRight } from 'lucide-react';
import { Link } from 'react-router-dom';
import RepoCard from './RepoCard';
import { Button } from '../ui/button';
import GalleryCard from '../dashboard/GalleryCard';
import { Bookmark } from '/imports/api/bookmarks';

// import { console } from 'inspector';

// const mockRepos = [
//   { id: 1, repoName: 'Repository One', repoDescription: 'Short description of repo one.' },
//   { id: 2, repoName: 'Repository Two', repoDescription: 'Short description of repo two.' },
//   { id: 3, repoName: 'Repository Three', repoDescription: 'Another short description.' },
//   { id: 4, repoName: 'Repository Four', repoDescription: 'Yet another description.' },
// ];

function LastSavedRepos() {
  const [bookmarks, setBookmarks] = useState<Bookmark[]>([]);

  useEffect(() => {
    Meteor.call('bookmarks.getNBookmarks', 2, (error: Error | undefined, result: React.SetStateAction<Bookmark[]>) => {
      if (error) {
        console.error('Error fetching bookmarks:', error);
      } else {
        setBookmarks(result);
      }
    });
  }, [bookmarks]);

  return (
    <div className="w-full max-w-4xl mt-8 p-6 bg-git-bg-secondary/20 dark:bg-git-bg-secondary border border-border rounded-lg">
      <h2 className="text-2xl font-semibold text-foreground mb-6 text-center">Last Saved Repositories</h2>
      <div className="flex flex-row justify-evenly">
        {bookmarks.map((repo) => (
          <GalleryCard
            bookmark={repo}
            onclick={() => { console.log('To be implemented'); }}
          />
        ))}
      </div>
      <div className="flex justify-end mt-6">
        <Button 
        variant={'secondary'}
        >
          <Link to="/dashboard">See All</Link>
          {' '}
          <ChevronRight className="ml-1 h-4 w-4" />
        </Button>
      </div>
    </div>
  );
}

export default LastSavedRepos;
