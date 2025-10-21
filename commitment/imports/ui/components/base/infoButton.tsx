"use client"

import React from "react"
import { Info } from "lucide-react"
import { Button } from "@base/button"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@base/tooltip"
import { cn } from "@ui/lib/utils"

interface InfoButtonProps {
  description: string
  className?:string
}

const InfoButton: React.FC<InfoButtonProps> = ({ description,className }) => {
  return (
    <TooltipProvider>
      <Tooltip>
        <TooltipTrigger asChild>        
          <Button
          variant="ghost"
          size="icon"
          className={cn("rounded-full! h-[18px] w-[18px] p-0 mt-4",className)}
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
