import React, { useState } from "react";
import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "@ui/components/ui/navigation-menu";
import { navigationMenuTriggerStyle } from "@ui/components/ui/navigation-menu";
import SignUpButton from "./signUpButton";
import ProfileMenu from "@ui/components/ui/profile-menu";
import { Moon, Sun } from "lucide-react";
import { useLocation, Link } from "react-router-dom";
interface NavBarProps {
  isLoggedIn: boolean;
}

export const NavBar: React.FC<NavBarProps> = ({ isLoggedIn }) => {
  const [isDarkMode, setIsDarkMode] = useState(false);
  const location = useLocation();
  const isLandingPage = location.pathname === "/";

  const handleSignOut = () => {
    // note to self: implement signing out logic here later
    console.log("Sign out clicked");
  };

  const handleToggleDarkMode = () => {
    setIsDarkMode(!isDarkMode);
    // note to self: implement dark mode logic here later
    console.log("Dark mode toggled:", !isDarkMode);
  };

  return (
    <div
      className={`z-50 flex items-center justify-between py-2 border-b git-bg-bottom
        ${
          isLandingPage
            ? "sticky top-0 px-4 rounded-md shadow-lg  ml-32 mr-32"
            : "relative px-4"
        }
      `}
    >
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <a className="flex items-center space-x-3">
            <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
            <span className="text-xl"></span>
          </a>

          {isLandingPage ? (
            // Landing page navigation items
            <>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#about"
                  className={navigationMenuTriggerStyle()}
                >
                  About
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#features"
                  className={navigationMenuTriggerStyle()}
                >
                  Features
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#howto"
                  className={navigationMenuTriggerStyle()}
                >
                  How-To
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          ) : (
            // App navigation items
            <>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <Link
                    to="/dashboard"
                    className={navigationMenuTriggerStyle()}
                  >
                    Dashboard
                  </Link>{" "}
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink>
                  <a className={navigationMenuTriggerStyle()}>Docs</a>
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          )}
        </NavigationMenuList>
      </NavigationMenu>

      <div className="flex items-center space-x-4">
        <button
          onClick={handleToggleDarkMode}
          className="p-2 rounded-full hover:bg-gray-200 transition-colors"
          aria-label={
            isDarkMode ? "Switch to light mode" : "Switch to dark mode"
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
            <a href="/login" className={navigationMenuTriggerStyle()}>
              Login
            </a>
            <SignUpButton />
          </>
        )}
        {isLoggedIn && <ProfileMenu onSignOut={handleSignOut} />}
      </div>
    </div>
  );
};

export default NavBar;
