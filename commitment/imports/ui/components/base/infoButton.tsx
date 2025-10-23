"use client"

import React from "react"
import { Info } from "lucide-react"
import { Button } from "@base/button"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@base/tooltip"
import { cn } from "@ui/lib/utils"

interface InfoButtonProps {
  description: string
  className?:string
  variant?: 'default' | 'muted' | 'active'
}

const InfoButton: React.FC<InfoButtonProps> = ({ description, className, variant = 'default' }) => {
  const getVariantStyles = () => {
    switch (variant) {
      case 'muted':
        return "text-muted-foreground hover:text-foreground"
      case 'active':
        return "text-foreground"
      default:
        return "text-foreground"
    }
  }

  return (
    <TooltipProvider>
      <Tooltip>
        <TooltipTrigger asChild>        
          <Button
          variant="ghost"
          size="icon"
          className={cn(
            "rounded-full! h-[18px] w-[18px] p-0 mt-4 transition-colors",
            getVariantStyles(),
            className
          )}
        >
          <Info className="w-6! h-6!" />
        </Button>
        </TooltipTrigger>
        <TooltipContent side="top" align="center" className="max-w-sm">
          <p className="text-center">  {description}
          </p>
        </TooltipContent>
      </Tooltip>
    </TooltipProvider>


  )
}

export default InfoButton
