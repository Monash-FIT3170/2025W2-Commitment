import React, { useState } from 'react';
import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  navigationMenuTriggerStyle,
} from '@ui/components/ui/navigation-menu';
import ProfileMenu from '@ui/components/ui/profile-menu';
import { Moon, Sun } from 'lucide-react';
import { useLocation, Link } from 'react-router-dom';
import SignUpButton from './signUpButton';

interface NavBarProps {
  isLoggedIn: boolean;
}

function NavBar({ isLoggedIn }: NavBarProps) {
  const [isDarkMode, setIsDarkMode] = useState(false);
  const location = useLocation();
  const isLandingPage = location.pathname === '/';

  const handleSignOut = () => {
    // note to self: implement signing out logic here later
    console.log('Sign out clicked');
  };

  const handleToggleDarkMode = () => {
    setIsDarkMode(!isDarkMode);
    // note to self: implement dark mode logic here later
    console.log('Dark mode toggled:', !isDarkMode);
  };

  return (
    <div
      className={`z-50 flex items-center justify-between py-2 border-b bg-white  
        ${isLandingPage ? 'sticky top-0 px-4 rounded-md shadow-lg  ml-32 mr-32' : 'relative px-4'}
      `}
    >
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <div className="flex items-center space-x-3">
            <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
            <span className="text-xl text-gray-900" />
          </div>

          {isLandingPage ? (
            // Landing page navigation items
            <>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <div href="#about" className={navigationMenuTriggerStyle()}>
                    About
                  </div>
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <div href="#features" className={navigationMenuTriggerStyle()}>
                    Features
                  </div>
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <div href="#howto" className={navigationMenuTriggerStyle()}>
                    How-To
                  </div>
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          ) : (
            // App navigation items
            <>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <Link to="/dashboard" className={navigationMenuTriggerStyle()}>
                    Dashboard
                  </Link>
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <div className={navigationMenuTriggerStyle()}>Docs</div>
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          )}
        </NavigationMenuList>
      </NavigationMenu>

      <div className="flex items-center space-x-4">
        <button
          type="button"
          onClick={handleToggleDarkMode}
          className="p-2 rounded-full hover:bg-gray-200 transition-colors"
          aria-label={
            isDarkMode ? 'Switch to light mode' : 'Switch to dark mode'
          }
        >
          {isDarkMode ? (
            <Sun className="h-6 w-6 text-yellow-400" />
          ) : (
            <Moon className="h-6 w-6 text-gray-600" />
          )}
        </button>

        {!isLoggedIn && (
          <>
            <div href="/login" className={navigationMenuTriggerStyle()}>
              Login
            </div>
            <SignUpButton />
          </>
        )}
        {isLoggedIn && <ProfileMenu onSignOut={handleSignOut} />}
      </div>
    </div>
  );
};

export default NavBar;
