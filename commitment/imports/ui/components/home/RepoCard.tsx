import React from "react";
import { Repository } from '@api/repositories';
import { Bookmark } from "@api/bookmarks"; 
import BookmarkButton from "../dashboard/BookmarkButton";

interface RepoCardProps {
  // accept either Repository or Bookmark
  repository: Repository | Bookmark;
  onClick: () => void;
}

function RepoCard({ repository, onClick }: RepoCardProps) {
  // Normalise fields so UI stays identical to main page card
  const name =
    "name" in repository ? repository.name : repository.title;

  const owner =
    "owner" in repository ? repository.owner : "Repository";

  const description =
    "description" in repository && repository.description
      ? repository.description
      : // fallback to something meaningful for bookmarks
        ("title" in repository ? repository.title : undefined);

  const url = "url" in repository ? repository.url : "";

  const createdAt = repository.createdAt;

  const metadata =
    "metadata" in repository ? repository.metadata : undefined;

  return (
    <div
      className="relative bg-git-card-primary p-4 rounded-lg border border-git-stroke-primary cursor-pointer transition-all duration-200 hover:shadow-md"
      onClick={onClick}
    >
      <div className="flex items-start justify-between mb-3">
        <div className="flex-1">
          <h3 className="font-semibold text-git-card-text text-lg mb-1">
            {name}
          </h3>
          <p className="text-sm text-git-card-text mb-2">
            {owner}
          </p>
          {description && (
            <p className="text-sm text-git-card-text mb-3 line-clamp-2">
              {description}
            </p>
          )}
        </div>

        {/* <span className={`px-2 py-1 rounded-full text-xs font-medium ${getStatusColor(repository.analysisStatus)}`}>
          {getStatusText(repository.analysisStatus)}
        </span> */}
        <span
          className="flex space-x-2 absolute right-4 top-4"
          onClick={(e) => e.stopPropagation()}
        >
          <BookmarkButton
            url={url}
            title={name}
            variant="primary"
          />
        </span>
      </div>

      <div className="flex items-center justify-between text-xs text-git-card-text">
        <span>Added: {createdAt.toLocaleDateString()}</span>
        {metadata && (
          <div className="flex items-center space-x-2">
            {metadata.language && (
              <span className="bg-git-card-secondary text-git-card-text px-2 py-1 rounded">
                {metadata.language}
              </span>
            )}
            {metadata.stars && <span>⭐ {metadata.stars}</span>}
          </div>
        )}
      </div>
    </div>
  );
}

export default RepoCard;
