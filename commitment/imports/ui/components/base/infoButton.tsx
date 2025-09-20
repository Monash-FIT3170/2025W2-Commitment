"use client"

import React from "react"
import { Info } from "lucide-react"
import { Button } from "../ui/button"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "../ui/tooltip"

interface InfoButtonProps {
  description: string
}

const InfoButton: React.FC<InfoButtonProps> = ({ description }) => {
  return (
    <TooltipProvider>
      <Tooltip>
        <TooltipTrigger asChild>        
          <Button
          variant="ghost"
          size="icon"
          className="rounded-full! h-[18px] w-[18px] p-0 mt-4"
        >
          <Info className="w-6! h-6!" />
        </Button>
        </TooltipTrigger>
        <TooltipContent>
          <p>  {description}
          </p>
        </TooltipContent>
      </Tooltip>
    </TooltipProvider>


  )
}

export default InfoButton
