import React, { useState, KeyboardEvent } from 'react';
import { cn } from '@ui/lib/utils';
import { useNavigate } from 'react-router-dom';

// Define supported Git platforms
interface GitPlatform {
    name: string;
    domain: string;
    httpsRegex: RegExp;
    sshRegex: RegExp;
    apiEndpoint: string;
    extractOwnerRepo: (url: string) => { owner: string; repo: string } | null;
}

const SUPPORTED_PLATFORMS: GitPlatform[] = [
    {
        name: 'GitHub',
        domain: 'github.com',
        httpsRegex: /^https:\/\/github\.com\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+(\.git)?$/,
        sshRegex: /^git@github\.com:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/,
        apiEndpoint: 'https://api.github.com/repos',
        extractOwnerRepo: (url: string) => {
            const match = url.match(/github\.com\/([^\/]+)\/([^\/]+?)(?:\.git)?$/);
            return match ? { owner: match[1], repo: match[2] } : null;
        }
    },
    {
        name: 'GitLab',
        domain: 'gitlab.com',
        httpsRegex: /^https:\/\/gitlab\.com\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+(\.git)?$/,
        sshRegex: /^git@gitlab\.com:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/,
        apiEndpoint: 'https://gitlab.com/api/v4/projects',
        extractOwnerRepo: (url: string) => {
            const match = url.match(/gitlab\.com\/([^\/]+)\/([^\/]+?)(?:\.git)?$/);
            return match ? { owner: match[1], repo: match[2] } : null;
        }
    },
    {
        name: 'Bitbucket',
        domain: 'bitbucket.org',
        httpsRegex: /^https:\/\/[a-zA-Z0-9_-]*@?bitbucket\.org\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+(\.git)?$/,
        sshRegex: /^git@bitbucket\.org:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/,
        apiEndpoint: 'https://api.bitbucket.org/2.0/repositories',
        extractOwnerRepo: (url: string) => {
            // Handle both HTTPS and SSH URLs:
            // HTTPS: https://bitbucket.org/owner/repo.git or https://username@bitbucket.org/owner/repo.git
            // SSH: git@bitbucket.org:owner/repo.git
            let match;
            if (url.includes('git@')) {
                // SSH format: git@bitbucket.org:owner/repo.git
                match = url.match(/git@bitbucket\.org:([^\/]+)\/([^\/]+?)(?:\.git)?$/);
            } else {
                // HTTPS format: https://bitbucket.org/owner/repo.git or https://username@bitbucket.org/owner/repo.git
                match = url.match(/bitbucket\.org\/(?:[^\/]+@)?([^\/]+)\/([^\/]+?)(?:\.git)?$/);
            }
            return match ? { owner: match[1], repo: match[2] } : null;
        }
    }
];

const GitRepoInputSection = () => {
    const navigate = useNavigate();
    const [repoUrl, setRepoUrl] = useState('');
    const [validationError, setValidationError] = useState<string | null>(null);
    const [isValidating, setIsValidating] = useState(false);

    const detectPlatform = (url: string): GitPlatform | null => {
        return SUPPORTED_PLATFORMS.find(platform => 
            url.includes(platform.domain)
        ) || null;
    };

    const validateRepoUrl = async (url: string): Promise<string | null> => {
        const trimmedUrl = url.trim();
        const cleanUrl = trimmedUrl.startsWith('@') ? trimmedUrl.substring(1) : trimmedUrl;

        if (!cleanUrl) {
            return "Repository link cannot be empty.";
        }

        // Detect which platform this URL belongs to
        const platform = detectPlatform(cleanUrl);
        if (!platform) {
            return `Unsupported Git platform. Currently supported: ${SUPPORTED_PLATFORMS.map(p => p.name).join(', ')}`;
        }

        // Check if URL format is valid for the detected platform
        const isHttpsValid = platform.httpsRegex.test(cleanUrl);
        const isSshValid = platform.sshRegex.test(cleanUrl);

        if (!isHttpsValid && !isSshValid) {
            return `Invalid ${platform.name} repository link format. Please use one of these formats:\n` +
                   `- HTTPS: https://${platform.domain}/username/repo or https://${platform.domain}/username/repo.git\n` +
                   `- SSH: git@${platform.domain}:username/repo.git`;
        }

        // Extract owner and repo for API validation
        const ownerRepo = platform.extractOwnerRepo(cleanUrl);
        if (!ownerRepo) {
            return `Invalid ${platform.name} URL format.`;
        }

        // Validate repository exists using platform-specific API
        try {
            let apiUrl: string;
            let response: Response;

            switch (platform.name) {
                case 'GitHub':
                    apiUrl = `${platform.apiEndpoint}/${ownerRepo.owner}/${ownerRepo.repo}`;
                    response = await fetch(apiUrl);
                    break;
                
                case 'GitLab':
                    // GitLab API uses encoded path: owner%2Frepo
                    const encodedPath = `${ownerRepo.owner}%2F${ownerRepo.repo}`;
                    apiUrl = `${platform.apiEndpoint}/${encodedPath}`;
                    response = await fetch(apiUrl);
                    break;
                
                case 'Bitbucket':
                    apiUrl = `${platform.apiEndpoint}/${ownerRepo.owner}/${ownerRepo.repo}`;
                    response = await fetch(apiUrl);
                    break;
                
                default:
                    return `Validation not implemented for ${platform.name}`;
            }

            if (!response.ok) {
                return `${platform.name} repository not found. Please check the URL and try again.`;
            }

            return null; // URL is valid and repository exists
        } catch (error) {
            return `Unable to validate ${platform.name} repository. Please check your internet connection.`;
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
                placeholder="Insert repo link (GitHub, GitLab, Bitbucket)" 
                className={cn(
                    "w-full max-w-lg px-4 py-2 border rounded-full shadow-xs focus:outline-hidden",
                    validationError ? 'border-red-500 focus:ring-red-500 focus:border-red-500' : 'border-[#F1502F] focus:ring-[#F1502F] focus:border-red-500',
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