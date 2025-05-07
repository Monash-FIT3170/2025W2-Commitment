import React from "react";
import { Button } from "@ui/components/ui/button";
import { cn } from "@ui/lib/utils";

interface SignUpButtonProps {
  className?: string;
}

const SignUpButton: React.FC<SignUpButtonProps> = ({ className }) => (
  <Button
    className={cn(
      "w-[100px] h-auto text-white rounded-full  text-center bg-[#F1502F] hover:bg-[#F1502F]  drop-shadow-lg",
      className
    )}
  >
    Sign Up
  </Button>
);

export default SignUpButton;
