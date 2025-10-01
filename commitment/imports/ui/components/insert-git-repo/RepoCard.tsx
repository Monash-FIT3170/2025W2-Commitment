import React from "react";
import { Repository } from "/imports/api/repositories";
import BookmarkButton from "../dashboard/BookmarkButton";

interface RepoCardProps {
  repository: Repository;
  onClick: () => void;
}

function RepoCard({ repository, onClick }: RepoCardProps) {
  const getStatusColor = (status: string) => {
    switch (status) {
      case "completed":
        return "bg-green-100 text-green-800";
      case "failed":
        return "bg-red-100 text-red-800";
      case "pending":
      default:
        return "bg-yellow-100 text-yellow-800";
    }
  };

  const getStatusText = (status: string) => {
    switch (status) {
      case "completed":
        return "Completed";
      case "failed":
        return "Failed";
      case "pending":
      default:
        return "Pending";
    }
  };

  return (
    <div
      className="relative bg-git-bg-elevated p-4 rounded-lg border border-git-text-primary cursor-pointer transition-all duration-200 hover:shadow-md"
      onClick={onClick}
    >
      <div className="flex items-start justify-between mb-3">
        <div className="flex-1">
          <h3 className="font-semibold text-git-text-primary text-lg mb-1">
            {repository.name}
          </h3>
          <p className="text-sm text-git-text-secondary mb-2">
            {repository.owner}
          </p>
          {repository.description && (
            <p className="text-sm text-git-text-secondary mb-3 line-clamp-2">
              {repository.description}
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
            url={repository.url}
            title={repository.name}
            variant="secondary"
          />
        </span>
      </div>

      <div className="flex items-center justify-between text-xs text-git-text-secondary">
        <span>Added: {repository.createdAt.toLocaleDateString()}</span>
        {repository.metadata && (
          <div className="flex items-center space-x-2">
            {repository.metadata.language && (
              <span className="bg-blue-100 text-blue-800 px-2 py-1 rounded">
                {repository.metadata.language}
              </span>
            )}
            {repository.metadata.stars && (
              <span>⭐ {repository.metadata.stars}</span>
            )}
          </div>
        )}
      </div>
    </div>
  );
}

export default RepoCard;
