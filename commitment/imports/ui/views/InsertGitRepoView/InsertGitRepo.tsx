import React, { useState } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Logo } from '@ui/components/landing-page/MainPage';
import { cn } from "@ui/lib/utils";
import LastSavedRepos from '@ui/components/insert-git-repo/LastSavedRepos';
import GitRepoInputSection from '@ui/components/insert-git-repo/GitRepoInputSection';

const InsertGitRepo = () => {
    //const [isLoggedIn, setIsLoggedIn] = useState(true); // use this to test the logged in state
    const [isLoggedIn, setIsLoggedIn] = useState(false); // use this to test the logged out state

    return (
        <div>
            <NavBar isLoggedIn={isLoggedIn} />
            
            {isLoggedIn ? (
                <div className="flex flex-col items-center pt-20">
                    <h1 className="text-8xl text-gray-700 mb-8">Welcome Back, Baset</h1>
                    <GitRepoInputSection />
                    <LastSavedRepos />
                </div>
            ) : (
                <div className="flex flex-col items-center pt-20">
                    <div className="mb-4">
                        <Logo />
                    </div>
                    <p className="text-center text-lg text-gray-700 mb-8">Quick Git metrics for overworked TAs</p>
                    <GitRepoInputSection />
                    <p className="text-center text-lg text-gray-700 mt-8">Need to quickly understand how committed your students are to their group? We got you</p>
                </div>
            )}
        </div>
    )
}

export default InsertGitRepo;