import React from "react";
import { Moon, Sun, Github } from "lucide-react";
import { Link, useLocation } from "react-router-dom";
import { Accounts } from "meteor/accounts-base";
import { useTheme } from "@hook/useTheme";
import {
  navigationMenuTriggerStyle,
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
} from "@base/navigation-menu";
import { useAuth } from "@hook/useAuth";
import { Button } from "@base/button";
import {ProfileMenu} from "./profile-menu";

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
        <NavigationMenuList className="flex items-center space-x-4">
          <NavigationMenuItem>
            <NavigationMenuLink asChild>
              <Link to={isLandingPage ? "/" : "/home"} className="inline-flex items-center">
                <img src="/logo.svg" alt="Logo" className="h-10 w-10" />
              </Link>
            </NavigationMenuLink>
          </NavigationMenuItem>

          {isLandingPage ? (
            // Landing page navigation items
            <>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#about"
                  className={navigationMenuTriggerStyle({ kind: "link" })}
                >
                  About
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#features"
                  className={navigationMenuTriggerStyle({ kind: "link" })}
                >
                  Features
                </NavigationMenuLink>
              </NavigationMenuItem>
              <NavigationMenuItem>
                <NavigationMenuLink
                  href="#howto"
                  className={navigationMenuTriggerStyle({ kind: "link" })}
                >
                  How-To
                </NavigationMenuLink>
              </NavigationMenuItem>
            </>
          ) : (
            // App navigation items
            <>
                <NavigationMenuItem>
                  <NavigationMenuLink asChild>
                    <Link to="/dashboard" className={navigationMenuTriggerStyle({ kind: "link" })}>
                      Dashboard
                    </Link>
                  </NavigationMenuLink>
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
          className={navigationMenuTriggerStyle({ kind: "icon" })}
          aria-label={isDark ? "Switch to light mode" : "Switch to dark mode"}
        >
          {isDark ? (
            <Sun className="h-6 w-6 text-yellow-400" />
          ) : (
            <Moon className="h-6 w-6" />
          )}
        </button>

        {/* Project's Github link */}  
        <a
          href="https://github.com/Monash-FIT3170/2025W2-Commitment"
          target="_blank"
          rel="noopener noreferrer"
          className={navigationMenuTriggerStyle({ kind: "icon" })}
          aria-label="Project GitHub"
        >
          <Github className="h-6 w-6" />
        </a>

        {!isLoggedIn && (
          <div className="flex items-center space-x-3">
            <a href="/login" className={navigationMenuTriggerStyle({ kind: "link" })}>
              Log in
            </a>
            {/* Sign up button */}
            <Button
              className="font-mono w-[100px] h-auto text-white rounded-full text-center bg-git-int-primary hover:bg-git-int-primary-hover drop-shadow-lg"
              asChild
            >
              <a href="/signup">Sign Up</a>
            </Button>
          </div>
        )}
        {isLoggedIn && <ProfileMenu onSignOut={handleSignOut} />}
      </div>
    </div>
  );
};

export default NavBar;