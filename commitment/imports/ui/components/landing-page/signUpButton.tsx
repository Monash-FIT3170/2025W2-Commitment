import React from "react";
import { Button } from "../ui/button";
import { cn } from "@ui/lib/utils";

interface SignUpButtonProps {
  className?: string;
}

const SignUpButton: React.FC<SignUpButtonProps> = ({ className }) => (
  <Button
    className={cn(
      "font-mono w-[100px] h-auto text-white rounded-full  text-center bg-git-int-primary hover:bg-git-int-primary-hover drop-shadow-lg",
      className
    )}
  >
    Sign Up
  </Button>
);

export default SignUpButton;
