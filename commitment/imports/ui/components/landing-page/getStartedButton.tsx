import React from "react";
import { Button } from "@ui/components/ui/button";
import { cn } from "@ui/lib/utils"

interface GetStartedButtonProps {
  className?: string;
}

const GetStartedButton: React.FC<GetStartedButtonProps> = ({
  className
}) => (
  <Button className={cn("font-mono w-[341px] h-auto text-[36px] rounded-full  text-center bg-[#F1502F] hover:bg-[#F1502F]  drop-shadow-lg",className)}>
    Get Started
  </Button>
  // <button
  // className="w-[341px] h-[87px] bg-cover bg-center rounded-full flex items-center justify-center">
  // <p className="text-[36px] text-gray-700 ">Get Started</p>

  // </button>
);

export default GetStartedButton;
