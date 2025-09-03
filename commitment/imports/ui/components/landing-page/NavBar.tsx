import React from "react";
import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "../ui/navigation-menu";
import { navigationMenuTriggerStyle } from "../ui/navigation-menu";
import SignUpButton from "./SignUpButton";
import ProfileMenu from "../ui/profile-menu";
import {  Moon, Sun } from "lucide-react";
import { Link, useLocation } from "react-router-dom";
import { Accounts } from "meteor/accounts-base";
import { useTheme } from "@ui/hooks/useTheme";
interface NavBarProps {
  isLoggedIn: boolean;
}

export const NavBar: React.FC<NavBarProps> = ({ isLoggedIn }) => {
  const { isDark, toggle } = useTheme();

  const location = useLocation();
  const isLandingPage = location.pathname === "/";


  const handleSignOut = () => {
    // TODO: implement signing out logic here later
    console.log("Sign out clicked");

    Accounts.logout(() => {
      console.log("Signed out.")
    });
  };

  const handleToggleDarkMode = () => {
    toggle();
    // note to self: implement dark mode logic here later
    console.log("Dark mode toggled:", !isDark);
  };

  return (
    <div
      className="z-50 flex items-center justify-between py-2 border-b bg-git-bg-elevated  sticky top-0 px-4 rounded-md shadow-lg  ml-32 mr-32"

    >
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <div className="flex items-center space-x-3">
            <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
          </div>

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
                <Link to="/dashboard" className={navigationMenuTriggerStyle()}>
                  Dashboard
                </Link>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink className={navigationMenuTriggerStyle()}>
                  Docs
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          )}
        </NavigationMenuList>
      </NavigationMenu>


      {/* Right hand side nav */}
      <div className="flex items-center space-x-4 ">
        <button
          type="button"
          onClick={handleToggleDarkMode}
          className="p-2 rounded-full hover:bg-git-bg-bottom/30 transition-colors"
          aria-label={
            isDark ? "Switch to light mode" : "Switch to dark mode"
          }
        >
          {isDark ? (
            <Sun className="h-6 w-6 text-yellow-400" />
          ) : (
            <Moon className="h-6 w-6 text-gray-600" />
          )}
        </button>

        {!isLoggedIn && (
          <div>
            <a href="/login" className={navigationMenuTriggerStyle()}>
              Log in
            </a>
            <SignUpButton />
          </div>
        )}
        {isLoggedIn && <ProfileMenu onSignOut={handleSignOut} />}
      </div>
    </div>

  );
}

export default NavBar;
