"use client"

import React from "react"
import { Info } from "lucide-react"
import { Button } from "@ui/components/ui/button"
import {
  Popover,
  PopoverTrigger,
  PopoverContent,
} from "@ui/components/ui/popover"

interface InfoButtonProps {
  description: string
}

const InfoButton: React.FC<InfoButtonProps> = ({ description }) => {
  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button
          variant="ghost"
          size="icon"
          className="rounded-full! h-[18px] w-[18px] p-0 mt-4"
        >
          <Info className="w-6! h-6!" />
        </Button>
      </PopoverTrigger>
      <PopoverContent
  side="top"
  align="center"
  className="relative bg-white border border-gray-300 shadow-lg rounded-lg px-4 py-3 max-w-xs text-sm text-gray-700"
  style={{ transform: 'translateY(-14px)' }}
>
  {/* Speech bubble tail centered and moved up */}
  <div
    className="absolute -bottom-2 left-1/2 -translate-x-1/2 w-0 h-0
               border-t-10 border-t-white border-x-10 border-x-transparent"
  />
  {description}
</PopoverContent>



    </Popover>
  )
}

export default InfoButton
