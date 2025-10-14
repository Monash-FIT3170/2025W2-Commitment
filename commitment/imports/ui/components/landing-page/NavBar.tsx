import React from "react";
import { Moon, Sun } from "lucide-react";
import { Link, useLocation } from "react-router-dom";
import { Accounts } from "meteor/accounts-base";
import { useTheme } from "@ui/hooks/useTheme";
import ProfileMenu from "../ui/profile-menu";
import {
  navigationMenuTriggerStyle,
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "../ui/navigation-menu";
import { useAuth } from "../../hooks/useAuth";
import { Button } from "../ui/button";

export const NavBar: React.FC = () => {
  const { isDark, toggle } = useTheme();

  const location = useLocation();
  const isLandingPage = location.pathname === "/";

  const isLoggedIn = useAuth();

  const handleSignOut = () => {
    Accounts.logout(() => {
      console.log("Signed out.");
    });
  };

  const handleToggleDarkMode = () => {
    toggle();
  };

  return (
    <div className="sticky top-0 z-50 flex items-center justify-between py-2 border-b bg-git-bg-bottom relative px-4">
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <div className="flex items-center space-x-3">
            <NavigationMenuItem>
              <NavigationMenuLink>
                <Link to={isLandingPage ? "/" : "/home"}>
                  <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
                </Link>
              </NavigationMenuLink>
            </NavigationMenuItem>
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
              {/* <NavigationMenuItem>
                <NavigationMenuLink className={navigationMenuTriggerStyle()}>
                  Docs
                </NavigationMenuLink>
              </NavigationMenuItem> */}
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
          aria-label={isDark ? "Switch to light mode" : "Switch to dark mode"}
        >
          {isDark ? (
            <Sun className="h-6 w-6 text-yellow-400" />
          ) : (
            <Moon className="h-6 w-6 text-gray-600" />
          )}
        </button>

        {!isLoggedIn && (
          <div>
            <a href="/login" className={`${navigationMenuTriggerStyle()}mr-10`}>
              Log in
            </a>
            {/* Sign up button */}
            <Button
              className="font-mono w-[100px] h-auto text-white rounded-full  text-center bg-git-int-primary hover:bg-git-int-primary-hover drop-shadow-lg"
              asChild
            >
              <a href="/signup">Sign Up</a>
            </Button>{" "}
          </div>
        )}
        {isLoggedIn && <ProfileMenu onSignOut={handleSignOut} />}
      </div>
    </div>
  );
};

export default NavBar;
