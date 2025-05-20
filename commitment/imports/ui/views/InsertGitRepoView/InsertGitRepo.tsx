import React, { useState } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Logo } from '@ui/components/landing-page/MainPage';
import { Button } from '@ui/components/ui/button';
import { cn } from "@ui/lib/utils";
import LastSavedRepos from '@ui/components/insert-git-repo/LastSavedRepos';

const InsertGitRepo = () => {
    // const [isLoggedIn, setIsLoggedIn] = useState(true); // use this to test the logged in state
    const [isLoggedIn, setIsLoggedIn] = useState(false); // use this to test the logged out state
    const [repoUrl, setRepoUrl] = useState('');
    const [validationError, setValidationError] = useState<string | null>(null);

    const validateRepoUrl = (url: string): string | null => {
        // Regex for standard GitHub HTTPS and SSH formats
        const githubHttpsRegex = /^https:\/\/github\.com\/[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+$/;
        const githubSshRegex = /^git@github\.com:[a-zA-Z0-9_-]+\/[a-zA-Z0-9_-]+\.git$/;

        if (!url) {
            return "Repository link cannot be empty.";
        }

        if (githubHttpsRegex.test(url) || githubSshRegex.test(url)) {
            return null; // URL is valid
        } else {
            return "Invalid GitHub repository link format. Please use HTTPS (e.g., https://github.com/user/repo) or SSH (e.g., git@github.com:user/repo.git) format.";
        }
    };

    const handleAnalyseClick = () => {
        const error = validateRepoUrl(repoUrl);
        setValidationError(error);

        if (!error) {
            // TODO: Implement repository analysis logic here
            console.log('Valid repo URL:', repoUrl);
        }
    };

    return (
        <div>
            <NavBar isLoggedIn={isLoggedIn} />
            
            {isLoggedIn ? (
                <div className="flex flex-col items-center pt-20">
                    <h1 className="text-8xl text-gray-700 mb-8">Welcome Back, Baset</h1>
                    <input 
                        type="text" 
                        placeholder="Insert git repo link" 
                        className="w-full max-w-md px-4 py-2 border border-[#F1502F] rounded-full shadow-sm focus:outline-none focus:ring-[#F1502F] focus:border-[#F1502F] mb-4"
                        value={repoUrl}
                        onChange={(e) => setRepoUrl(e.target.value)}
                    />
                    <Button className={cn(
                        "w-[341px] h-auto text-white rounded-full text-center bg-[#F1502F] hover:bg-[#F1502F] drop-shadow-lg"
                    )}>
                        Analyse Repository
                    </Button>
                    <LastSavedRepos />
                </div>
            ) : (
                <div className="flex flex-col items-center pt-20">
                    <div className="mb-4">
                        <Logo />
                    </div>
                    <p className="text-center text-lg text-gray-700 mb-8">Quick Git metrics for overworked TAs</p>
                    <input 
                        type="text" 
                        placeholder="Insert git repo link" 
                        className={cn(
                            "w-full max-w-md px-4 py-2 border rounded-full shadow-sm focus:outline-none",
                            validationError ? 'border-red-500 focus:ring-red-500 focus:border-red-500' : 'border-[#F1502F] focus:ring-[#F1502F] focus:border-[#F1502F]',
                            'mb-4'
                        )}
                        value={repoUrl}
                        onChange={(e) => setRepoUrl(e.target.value)}
                    />
                    {validationError && (
                        <p className="text-red-500 text-sm mt-1">{validationError}</p>
                    )}
                    <Button 
                        className={cn(
                            "w-[341px] h-auto text-white text-2xl rounded-full text-center bg-[#F1502F] hover:bg-[#F1502F] mt-4"
                        )}
                        onClick={handleAnalyseClick}
                    >
                        Analyse Repository
                    </Button>
                    <p className="text-center text-lg text-gray-700 mt-8">Need to quickly understand how committed your students are to their group? We got you</p>
                </div>
            )}
        </div>
    )
}

export default InsertGitRepo;