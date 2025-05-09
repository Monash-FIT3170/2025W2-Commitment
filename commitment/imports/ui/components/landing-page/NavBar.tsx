import React from "react";
import {
  NavigationMenu,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  // NavigationMenuTrigger,
} from "@ui/components/ui/navigation-menu";
import { navigationMenuTriggerStyle } from "@ui/components/ui/navigation-menu";
import SignUpButton from "@ui/components/ui/signUpButton";

export const NavBar = () => {
  return (
    <div className="sticky top-0 z-50 flex items-center justify-between px-4 py-2 border-b bg-white rounded-md shadow-lg">
      <NavigationMenu>
        <NavigationMenuList className="flex space-x-4">
          <a className="flex items-center space-x-3">
            <img src="/logo.svg" alt="Logo" className="h-10 w-10"/>
            <span className="text-xl text-gray-900"></span>
          </a>
          <NavigationMenuItem>
            <NavigationMenuLink className={navigationMenuTriggerStyle()}>
              About
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink className={navigationMenuTriggerStyle()}>
              Features
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink className={navigationMenuTriggerStyle()}>
              How-To
            </NavigationMenuLink>
          </NavigationMenuItem>
          <NavigationMenuItem>
            <NavigationMenuLink className={navigationMenuTriggerStyle()}>
              Our Repository
            </NavigationMenuLink>
          </NavigationMenuItem>
        </NavigationMenuList>
      </NavigationMenu>

      <div className="flex items-center space-x-4">
        <a className={navigationMenuTriggerStyle()}>Login</a>
        <SignUpButton />
      </div>
    </div>
  );
};

export default NavBar;
