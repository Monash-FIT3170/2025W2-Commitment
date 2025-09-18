import React, { useState, useRef, useEffect } from "react";
import { HelpCircle, LogOut, User, ChevronDown, Users } from "lucide-react";
import { Button } from "./button";
import { Separator } from "./separator";
import { useNavigate } from "react-router-dom";

interface ProfileMenuProps {
  onSignOut: () => void;
}

export const ProfileMenu: React.FC<ProfileMenuProps> = ({
  onSignOut,
}) => {
  const [isOpen, setIsOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();

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

  const handleSettingsClick = () => {
    setIsOpen(false);
    navigate('/settings');
  };

  return (
    <div className="relative" ref={menuRef}>
      <Button 
        variant="ghost" 
        className=""
        onClick={() => setIsOpen(!isOpen)}
      >
        <User className="h-6 w-6 text-foreground" />
        <ChevronDown className={`h-4 w-4 text-foreground transition-transform ${isOpen ? 'rotate-180' : 'rotate-0'}`} />
      </Button>

      {isOpen && (
        <div className="absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-popover ring-1 ring-black ring-opacity-5 z-50">
          <div className="py-1">
            <button
              className="flex items-center gap-2 w-full px-4 py-2 text-sm text-popover-foreground hover:bg-white/20 hover:rounded-lg"
            >
              <HelpCircle className="h-4 w-4" />
              <span>Help</span>
            </button>

            <Separator />

            <button
              onClick={handleSettingsClick}
              className="flex items-center gap-2 w-full px-4 py-2 text-sm text-foreground hover:bg-gray-100"
            >
              <Users className="h-4 w-4" />
              <span>Alias Configuration</span>
            </button>

            <Separator />

            <Separator />

            <button
              className="flex items-center gap-2 w-full px-4 py-2 text-sm text-red-500 hover:bg-white/20 hover:rounded-lg"
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