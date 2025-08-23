import React from "react";
import { Button } from "@ui/components/ui/button";
import { cn } from "@ui/lib/utils";
import { useNavigate } from "react-router-dom";

interface GetStartedButtonProps {
  className?: string;
}

const GetStartedButton: React.FC<GetStartedButtonProps> = ({ className }) => {
  const navigate = useNavigate();

  return (
    <Button
      onClick={() => navigate("/home")}
      className={cn(
        "w-[341px] h-auto text-[36px] font-mono rounded-full text-center text-white bg-git-int-primary hover:bg-git-int-primary-hover drop-shadow-lg",
        className
      )}
    >
      Get Started
    </Button>
  );
}

export default GetStartedButton;
