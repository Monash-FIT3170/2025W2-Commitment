import React, { useState, KeyboardEvent } from 'react';
import { cn } from '@ui/lib/utils';
import { useNavigate } from 'react-router-dom';
import { Meteor } from 'meteor/meteor';
import { Button } from '../ui/button';

function GitRepoInputSection() {
  const navigate = useNavigate();
  const [repoUrl, setRepoUrl] = useState('');
  const [validationError, setValidationError] = useState<string | null>(null);
  const [isProcessing, setIsProcessing] = useState(false);

  const validateRepoUrl = (url: string): string | null => {
    const trimmedUrl = url.trim();
    const cleanUrl = trimmedUrl.startsWith('@') ? trimmedUrl.substring(1) : trimmedUrl;

    // regex for standard github HTTPS and SSH formats, allowing .git optionally for HTTPS
    const githubHttpsRegex = /^https:\/\/github\.com\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+(\.git)?$/;
    const githubSshRegex = /^git@github\.com:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/;

    if (!cleanUrl) {
      return 'Repository link cannot be empty.';
    }

    if (githubHttpsRegex.test(cleanUrl) || githubSshRegex.test(cleanUrl)) {
      return null; // URL is valid
    }
    return 'Invalid GitHub repository link format. Please use one of these formats:\n'
                   + '- HTTPS: https://github.com/username/repo or https://github.com/username/repo.git\n'
                   + '- SSH: git@github.com:username/repo.git';
  };

  const extractRepoInfo = (url: string): { name: string; owner: string } | null => {
    // Handle HTTPS URLs
    if (url.startsWith('https://github.com/')) {
      const parts = url.replace('https://github.com/', '').replace('.git', '').split('/');
      if (parts.length === 2) {
        return { owner: parts[0], name: parts[1] };
      }
    }
    
    // Handle SSH URLs
    if (url.startsWith('git@github.com:')) {
      const parts = url.replace('git@github.com:', '').replace('.git', '').split('/');
      if (parts.length === 2) {
        return { owner: parts[0], name: parts[1] };
      }
    }
    
    return null;
  };

  const handleAnalyseClick = async () => {
    const error = validateRepoUrl(repoUrl);
    setValidationError(error);

    if (!error) {
      setIsProcessing(true);
      
      try {
        // Extract repository information from URL
        const repoInfo = extractRepoInfo(repoUrl);
        if (!repoInfo) {
          setValidationError('Could not extract repository information from URL');
          setIsProcessing(false);
          return;
        }

        // Store repository in database
        Meteor.call(
          'repositories.storeUrl',
          repoUrl,
          repoInfo.name,
          repoInfo.owner,
          `Repository: ${repoInfo.owner}/${repoInfo.name}`,
          (err: any, result: string) => {
            if (err) {
              console.error('Error storing repository:', err);
              setValidationError('Failed to store repository. Please try again.');
              setIsProcessing(false);
            } else {
              console.log('Repository stored successfully:', result);
              // Navigate to loading page with repository info
              navigate('/loading', { 
                state: { 
                  repoUrl,
                  repoId: result,
                  repoName: repoInfo.name,
                  repoOwner: repoInfo.owner
                } 
              });
            }
          }
        );
      } catch (error) {
        console.error('Error processing repository:', error);
        setValidationError('An unexpected error occurred. Please try again.');
        setIsProcessing(false);
      }
    }
  };

  const handleInputKeyPress = (event: KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') {
      handleAnalyseClick();
    }
  };

  return (
    <>
      <input
        type="text"
        placeholder="Insert git repo link"
        className={cn(
          'w-full max-w-lg px-4 py-2 border rounded-full shadow-xs focus:outline-hidden',
          validationError ? 'border-red-500 focus:ring-red-500 focus:border-red-500' : 'border-[#F1502F] focus:ring-[#F1502F] focus:border-[#F1502F]',
          'mb-4',
        )}
        value={repoUrl}
        onChange={(e) => setRepoUrl(e.target.value)}
        onKeyPress={handleInputKeyPress}
        disabled={isProcessing}
      />
      {validationError && (
        <p className="text-red-500 text-sm mt-1">{validationError}</p>
      )}
      <Button
        className={cn(
          'w-[341px] h-auto text-white font-mono text-2xl rounded-full text-center bg-git-int-primary hover:bg-git-int-primary-hover mt-4',
        )}
        onClick={handleAnalyseClick}
        disabled={isProcessing}
      >
        {isProcessing ? 'Processing...' : 'Analyse Repository'}
      </Button>
    </>
  );
}

export default GitRepoInputSection;
