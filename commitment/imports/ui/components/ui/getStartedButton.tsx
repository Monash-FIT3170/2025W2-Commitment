import React from "react";
import { Button } from "@ui/components/ui/button";
import { cn } from "@ui/lib/utils"
import { useNavigate } from "react-router-dom";

interface GetStartedButtonProps {
  className?: string;
}

const GetStartedButton: React.FC<GetStartedButtonProps> = ({
  className
}) => {
  const navigate = useNavigate();

  return (
    <Button 
      onClick={() => navigate('/insert-git-repo')}
      className={cn("w-[341px] h-auto text-[36px] rounded-full text-center bg-[#F1502F] hover:bg-[#F1502F] drop-shadow-lg", className)}
    >
      Get Started
    </Button>
  );
};

export default GetStartedButton;
