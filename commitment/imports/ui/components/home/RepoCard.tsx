import React from 'react';
import { Repository } from '@api/repositories';

interface RepoCardProps {
  repository: Repository;
  onClick: () => void;
}

function RepoCard({ repository, onClick }: RepoCardProps) {
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'completed':
        return 'bg-green-100 text-green-800';
      case 'failed':
        return 'bg-red-100 text-red-800';
      case 'pending':
      default:
        return 'bg-yellow-100 text-yellow-800';
    }
  };

  const getStatusText = (status: string) => {
    switch (status) {
      case 'completed':
        return 'Completed';
      case 'failed':
        return 'Failed';
      case 'pending':
      default:
        return 'Pending';
    }
  };

  return (
    <div 
      className="bg-white p-4 rounded-lg border border-gray-200 hover:border-gray-300 cursor-pointer transition-all duration-200 hover:shadow-md"
      onClick={onClick}
    >
      <div className="flex items-start justify-between mb-3">
        <div className="flex-1">
          <h3 className="font-semibold text-gray-900 text-lg mb-1">
            {repository.name}
          </h3>
          <p className="text-sm text-gray-600 mb-2">
            {repository.owner}
          </p>
          {repository.description && (
            <p className="text-sm text-gray-700 mb-3 line-clamp-2">
              {repository.description}
            </p>
          )}
        </div>
        <span className={`px-2 py-1 rounded-full text-xs font-medium ${getStatusColor(repository.analysisStatus)}`}>
          {getStatusText(repository.analysisStatus)}
        </span>
      </div>
      
      <div className="flex items-center justify-between text-xs text-gray-500">
        <span>
          Added: {repository.createdAt.toLocaleDateString()}
        </span>
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
