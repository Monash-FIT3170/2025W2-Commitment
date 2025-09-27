import React from 'react';
import { Info } from 'lucide-react';
import { Button } from '@base/button';

import { Bookmark } from "@api/bookmarks";
import BookmarkButton from "./BookmarkButton";

export interface RepoRowProps {
  bookmark: Bookmark;
  onclick: () => void;
  onInfo?: () => void;
}

export default function RepoRow({ bookmark, onclick, onInfo }: RepoRowProps) {
  return (
    <li
      className="bg-[#F1502F] text-white rounded-xl flex items-center px-4 py-2 gap-3 shadow-xs transition-colors w-full sm:w-auto "
    >
      {/* repo name */}
      <span className="flex-1 truncate font-medium">{bookmark.title}</span>

      {/* info icon */}
      <button
        type="button"
        onClick={onInfo}
        aria-label="repository info"
        className="text-white/80 hover:text-white focus:outline-hidden"
      >
        <Info size={18} />
      </button>

      {/* bookmark */}
      <BookmarkButton url={bookmark.url} title={bookmark.title} />

      {/* view */}
      <Button className="rounded-full" variant="secondary" onClick={onclick}>
        {' '}
        View Repository
      </Button>
    </li>
  );
}
