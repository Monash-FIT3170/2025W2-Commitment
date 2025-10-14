import React from "react";
import { Button } from "@ui/components/ui/button";
import { cn } from "@ui/lib/utils";
import { useNavigate } from "react-router-dom";

interface GetStartedButtonProps {
  className?: string;
}

const GetStartedButton: React.FC<GetStartedButtonProps> = () => {
  const navigate = useNavigate();

  return (
    <Button
      onClick={() => navigate("/home")}
      className="w-[341px] h-auto text-[36px] font-mono rounded-4xl text-center "
    >
      Get Started
    </Button>
  );
};

export default GetStartedButton;
