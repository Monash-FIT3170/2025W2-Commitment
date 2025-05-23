import React, { useState, useRef, useEffect } from "react";
import { HelpCircle, LogOut, User, ChevronDown } from "lucide-react";
import { Button } from "./button";
import { Separator } from "./separator";

interface ProfileMenuProps {
  onSignOut: () => void;
}

export const ProfileMenu: React.FC<ProfileMenuProps> = ({
  onSignOut,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);

  // Close menu when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  return (
    <div className="relative" ref={menuRef}>
      <Button 
        variant="ghost" 
        className="relative h-10 w-10 rounded-full hover:bg-gray-100 flex items-center justify-center gap-1"
        onClick={() => setIsOpen(!isOpen)}
      >
        <User className="h-6 w-6 text-gray-600" />
        <ChevronDown className={`h-4 w-4 text-gray-600 transition-transform ${isOpen ? 'rotate-180' : 'rotate-0'}`} />
      </Button>

      {isOpen && (
        <div className="absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 z-50">
          <div className="py-1">
            <button
              className="flex items-center gap-2 w-full px-4 py-2 text-sm text-gray-700 hover:bg-gray-100"
            >
              <HelpCircle className="h-4 w-4" />
              <span>Help</span>
            </button>

            <Separator />

            <button
              className="flex items-center gap-2 w-full px-4 py-2 text-sm text-red-600 hover:bg-gray-100"
              onClick={onSignOut}
            >
              <LogOut className="h-4 w-4" />
              <span>Sign Out</span>
            </button>
          </div>
        </div>
      )}
    </div>
  );
};

export default ProfileMenu; 