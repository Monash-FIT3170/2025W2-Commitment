import React, { useState, KeyboardEvent } from 'react';
import { Button } from '../ui/button';
import { cn } from "@ui/lib/utils";
import { useNavigate } from 'react-router-dom';

const GitRepoInputSection = () => {
    const navigate = useNavigate();
    const [repoUrl, setRepoUrl] = useState('');
    const [validationError, setValidationError] = useState<string | null>(null);
    const [isValidating, setIsValidating] = useState(false);

    const validateRepoUrl = async (url: string): Promise<string | null> => {
        const trimmedUrl = url.trim();
        const cleanUrl = trimmedUrl.startsWith('@') ? trimmedUrl.substring(1) : trimmedUrl;

        // regex for standard github HTTPS and SSH formats, allowing .git optionally for HTTPS
        const githubHttpsRegex = /^https:\/\/github\.com\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+(\.git)?$/;
        const githubSshRegex = /^git@github\.com:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/;

        if (!cleanUrl) {
            return "Repository link cannot be empty.";
        }

        if (githubHttpsRegex.test(cleanUrl) || githubSshRegex.test(cleanUrl)) {
            // Format is valid, now check if the repository actually exists using GitHub API
            try {
                // Extract owner and repo from GitHub URL
                const match = cleanUrl.match(/github\.com\/([^\/]+)\/([^\/]+?)(?:\.git)?$/);
                if (!match) {
                    return "Invalid GitHub URL format.";
                }
                
                const [, owner, repo] = match;
                const apiUrl = `https://api.github.com/repos/${owner}/${repo}`;
                
                const response = await fetch(apiUrl);
                if (!response.ok) {
                    return "GitHub repository not found. Please check the URL and try again.";
                }
                return null; // URL is valid and repository exists
            } catch (error) {
                return "Unable to validate repository. Please check your internet connection.";
            }
        } else {
            return "Invalid GitHub repository link format. Please use one of these formats:\n" +
                   "- HTTPS: https://github.com/username/repo or https://github.com/username/repo.git\n" +
                   "- SSH: git@github.com:username/repo.git";
        }
    };

    const handleAnalyseClick = async () => {
        setIsValidating(true);
        setValidationError(null);
        
        try {
            const error = await validateRepoUrl(repoUrl);
            setValidationError(error);

            if (!error) {
                // TODO: Implement repository analysis logic here
                console.log('Valid repo URL:', repoUrl);
                navigate('/loading', { state: { repoUrl } });
            }
        } finally {
            setIsValidating(false);
        }
    };

    const handleInputKeyPress = async (event: KeyboardEvent<HTMLInputElement>) => {
        if (event.key === 'Enter') {
            await handleAnalyseClick();
        }
    };

    return (
        <>
            <input 
                type="text" 
                placeholder="Insert git repo link" 
                className={cn(
                    "w-full max-w-lg px-4 py-2 border rounded-full shadow-xs focus:outline-hidden",
                    validationError ? 'border-red-500 focus:ring-red-500 focus:border-red-500' : 'border-[#F1502F] focus:ring-[#F1502F] focus:border-[#F1502F]',
                    'mb-4'
                )}
                value={repoUrl}
                onChange={(e) => setRepoUrl(e.target.value)}
                onKeyPress={handleInputKeyPress}
            />
            {validationError && (
                <p className="text-red-500 text-sm mt-1">{validationError}</p>
            )}
            <Button 
                className={cn(
                    "w-[341px] h-auto text-white font-mono text-2xl rounded-full text-center bg-git-int-primary hover:bg-git-int-primary-hover mt-4",
                    isValidating && "opacity-50 cursor-not-allowed"
                )}
                onClick={handleAnalyseClick}
                disabled={isValidating}
            >
                {isValidating ? "Validating..." : "Analyse Repository"}
            </Button>
        </>
    );
};

export default GitRepoInputSection; 