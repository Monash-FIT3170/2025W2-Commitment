import React, { useState } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';
import { Logo } from '@ui/components/landing-page/MainPage';

const InsertGitRepo = () => {
    // const [isLoggedIn, setIsLoggedIn] = useState(true); // use this to test the logged in state
    const [isLoggedIn, setIsLoggedIn] = useState(false); // use this to test the logged out state

    return (
        <div>
            <NavBar isLoggedIn={isLoggedIn} />
            
            {isLoggedIn ? (
                <div>
                    <h1>Welcome back! User name </h1>
                    {}
                </div>
            ) : (
                <div className="flex flex-col items-center justify-center pt-20">
                    <div className="mb-4">
                        <Logo />
                    </div>
                    <h1>Welcome to Git Repo Page</h1>
                    {}
                </div>
            )}
        </div>
    )
}

export default InsertGitRepo;