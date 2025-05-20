import React, { useState } from "react";
import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "@ui/components/ui/navigation-menu";
import { navigationMenuTriggerStyle } from "@ui/components/ui/navigation-menu";
import SignUpButton from "@ui/components/ui/signUpButton";
import ProfileMenu from "@ui/components/ui/profile-menu";

interface NavBarProps {
  isLoggedIn: boolean;
}

export const NavBar: React.FC<NavBarProps> = ({ isLoggedIn }) => {
  const [isDarkMode, setIsDarkMode] = useState(false);

  const handleSignOut = () => {
    // not to seld: implement signing out logic here later
    console.log("Sign out clicked");
  };

  const handleToggleDarkMode = () => {
    setIsDarkMode(!isDarkMode);
    // note to self: implement dark mode logic here later
  };

  return (
    <div className="sticky top-0 z-50 flex items-center justify-between px-4 py-2 border-b bg-white rounded-md shadow-lg ml-32 mr-32">
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <a className="flex items-center space-x-3">
            <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
            <span className="text-xl text-gray-900"></span>
          </a>
          <NavigationMenuItem>
            <NavigationMenuLink>
              <a href="#about" className={navigationMenuTriggerStyle()}>
                About
              </a>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink>
              <a href="#features" className={navigationMenuTriggerStyle()}>
                Features
              </a>
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink>
              <a href="#howto" className={navigationMenuTriggerStyle()}>
                How-To
              </a>
            </NavigationMenuLink>
          </NavigationMenuItem>
        </NavigationMenuList>
      </NavigationMenu>

      <div className="flex items-center space-x-4">
        {!isLoggedIn && (
          <>
            <a className={navigationMenuTriggerStyle()}>Login</a>
            <SignUpButton />
          </>
        )}
        {isLoggedIn && (
          <ProfileMenu
            onSignOut={handleSignOut}
            onToggleDarkMode={handleToggleDarkMode}
            isDarkMode={isDarkMode}
          />
        )}
      </div>
    </div>
  );
};

export default NavBar;
