import React, { useState } from 'react';
import NavBar from '@ui/components/landing-page/NavBar';

const InsertGitRepo = () => {
    const [isLoggedIn, setIsLoggedIn] = useState(true); // use this to test the logged in state
    //const [isLoggedIn, setIsLoggedIn] = useState(false); // use this to test the logged out state

    return (
        <div>
            <NavBar isLoggedIn={isLoggedIn} />
            
            {isLoggedIn ? (
                <div>
                    <h1>Welcome back! User name </h1>
                    {}
                </div>
            ) : (
                <div>
                    <h1>Welcome to Git Repo Page</h1>
                    {}
                </div>
            )}
        </div>
    )
}

export default InsertGitRepo;