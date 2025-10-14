import React, { useState, KeyboardEvent } from 'react';
import { cn } from '@ui/lib/utils';
import { useNavigate } from 'react-router-dom';
import { Meteor } from 'meteor/meteor';
import { Button } from '../ui/button';
import { Input } from '../ui/input';

function GitRepoInputSection() {
  const navigate = useNavigate();
  const [repoUrl, setRepoUrl] = useState('');
  const [validationError, setValidationError] = useState<string | null>(null);
  const [isProcessing, setIsProcessing] = useState(false);
  const [isCheckingExistence, setIsCheckingExistence] = useState(false);

  // Platform configuration for GitHub, GitLab and Bitbucket support
  const GIT_PLATFORMS = {
    github: {
      name: 'GitHub',
      domains: ['github.com'],
      httpsPattern: /^https:\/\/github\.com\/[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+(?:\.git)?$/,
      sshPattern: /^git@github\.com:[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+\.git$/
    },
    gitlab: {
      name: 'GitLab',
      domains: ['gitlab.com'],
      httpsPattern: /^https:\/\/gitlab\.com\/[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+(?:\.git)?$/,
      sshPattern: /^git@gitlab\.com:[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+\.git$/
    },
    bitbucket: {
      name: 'Bitbucket',
      domains: ['bitbucket.org'],
      httpsPattern: /^https:\/\/(?:[a-zA-Z0-9_.~-]+@)?bitbucket\.org\/[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+(?:\.git)?$/,
      sshPattern: /^git@bitbucket\.org:[a-zA-Z0-9_.~-]+\/[a-zA-Z0-9_.~-]+\.git$/
    }
  };

  const detectPlatform = (url: string) => {
    for (const [key, platform] of Object.entries(GIT_PLATFORMS)) {
      if (platform.domains.some(domain => url.includes(domain))) {
        return { key, ...platform };
      }
    }
    return null;
  };

  const validateRepoUrl = (url: string): { isValid: boolean; error?: string; platform?: string } => {
    const trimmedUrl = url.trim();
    const cleanUrl = trimmedUrl.startsWith('@') ? trimmedUrl.substring(1) : trimmedUrl;

    if (!cleanUrl) {
      return { isValid: false, error: 'Repository link cannot be empty.' };
    }

    // Detect platform
    const platform = detectPlatform(cleanUrl);
    
    if (!platform) {
      return { 
        isValid: false, 
        error: 'Unsupported platform. Supported: GitHub, GitLab, Bitbucket' 
      };
    }

    // Validate against platform-specific patterns
    const isHttpsValid = platform.httpsPattern.test(cleanUrl);
    const isSshValid = platform.sshPattern.test(cleanUrl);

    if (isHttpsValid || isSshValid) {
      return { isValid: true, platform: platform.name };
    }

    // Generate helpful error message with examples
    const examples = platform.key === 'bitbucket' 
      ? [
          'https://bitbucket.org/user/repo',
          'https://bitbucket.org/user/repo.git',
          'https://username@bitbucket.org/user/repo.git',
          'git@bitbucket.org:user/repo.git'
        ]
      : [
          `https://${platform.domains[0]}/user/repo`,
          `https://${platform.domains[0]}/user/repo.git`,
          `git@${platform.domains[0]}:user/repo.git`
        ];

    return {
      isValid: false,
      error: `Invalid ${platform.name} repository URL format. Examples:\n${examples.map(ex => `- ${ex}`).join('\n')}`,
      platform: platform.name
    };
  };

  const extractRepoInfo = (url: string): { name: string; owner: string } | null => {
    // Detect platform first
    const platform = detectPlatform(url);
    
    if (!platform) {
      return null;
    }

    // Handle HTTPS URLs for all platforms
    for (const domain of platform.domains) {
      if (url.startsWith(`https://${domain}/`)) {
        // Remove username@ from URL if present (common in Bitbucket)
        const cleanUrl = url.replace(/^https:\/\/[^@]+@/, 'https://');
        const parts = cleanUrl.replace(`https://${domain}/`, '').replace('.git', '').split('/');
        if (parts.length === 2) {
          return { owner: parts[0], name: parts[1] };
        }
      }
    }

    // Handle SSH URLs for all platforms
    for (const domain of platform.domains) {
      if (url.startsWith(`git@${domain}:`)) {
        const parts = url.replace(`git@${domain}:`, '').replace('.git', '').split('/');
        if (parts.length === 2) {
          return { owner: parts[0], name: parts[1] };
        }
      }
    }

    // Fallback: Generic pattern matching for any Git URL
    const httpsMatch = url.match(/^https?:\/\/[^@]*@?[^\/]+\/([^\/]+)\/([^\/]+?)(?:\.git)?$/);
    if (httpsMatch) {
      return { owner: httpsMatch[1], name: httpsMatch[2] };
    }

    const sshMatch = url.match(/^git@[^:]+:([^\/]+)\/([^\/]+?)(?:\.git)?$/);
    if (sshMatch) {
      return { owner: sshMatch[1], name: sshMatch[2] };
    }
    
    return null;
  };

  const checkRepositoryExists = async (url: string): Promise<boolean> => {
    return new Promise((resolve) => {
      Meteor.call('repo.checkExists', url, (error: any, result: boolean) => {
        if (error) {
          console.error('Error checking repository existence:', error);
          resolve(false);
        } else {
          resolve(result);
        }
      });
    });
  };

  const handleAnalyseClick = async () => {
    const validation = validateRepoUrl(repoUrl);
    setValidationError(validation.error || null);

    if (validation.isValid) {
      setIsProcessing(true);
      setIsCheckingExistence(true);
      
      try {
        // Check if repository exists before processing
        const exists = await checkRepositoryExists(repoUrl);
        setIsCheckingExistence(false);
        
        if (!exists) {
          setValidationError('Repository not found or not accessible. Please check the URL and try again.');
          setIsProcessing(false);
          return;
        }

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
          `${validation.platform} Repository: ${repoInfo.owner}/${repoInfo.name}`,
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
                  repoOwner: repoInfo.owner,
                  platform: validation.platform
                } 
              });
            }
          }
        );
      } catch (error) {
        console.error('Error processing repository:', error);
        setValidationError('An unexpected error occurred. Please try again.');
        setIsProcessing(false);
        setIsCheckingExistence(false);
      }
    }
  };

  const handleInputKeyPress = (event: KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') {
      handleAnalyseClick();
    }
  };

  const handleInputChange = (value: string) => {
    setRepoUrl(value);
    setValidationError(null);
  };

  return (
    <>
      <div className="relative w-full max-w-lg mb-4">
        <Input
          type="text"
          placeholder="Insert repository URL (GitHub, GitLab, Bitbucket)"
          className={cn(
            'w-full bg-git-bg-elevated/50 px-4 py-2 border shadow-xs focus:outline-hidden',
            validationError ? 'border-red-500 focus:ring-red-500 focus:border-red-500' : 'border-git-int-primary focus:ring-git-int-primary focus:border-git-int-primary',
          )}
          value={repoUrl}
          onChange={(e) => handleInputChange(e.target.value)}
          onKeyPress={handleInputKeyPress}
          disabled={isProcessing}
        />
      </div>
      
      {validationError && (
        <p className="text-red-500 text-sm mt-1 mb-4">{validationError}</p>
      )}
      <Button
        className={cn(
          'w-[341px] h-auto text-white font-mono text-2xl rounded-full text-center bg-git-int-primary hover:bg-git-int-primary-hover mt-4',
        )}
        onClick={handleAnalyseClick}
        disabled={isProcessing}
      >
        {isCheckingExistence ? 'Checking repository...' : isProcessing ? 'Processing...' : 'Analyse Repository'}
      </Button>
    </>
  );
}

export default GitRepoInputSection;
