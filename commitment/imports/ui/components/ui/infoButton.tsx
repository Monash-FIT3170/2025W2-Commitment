import React from 'react';
import { Info } from 'lucide-react';
import { Button } from '@ui/components/ui/button'; 

const InfoButton: React.FC = () => {
  return (
    <Button variant="ghost" size="icon" className="!rounded-full h-[18px] w-[18px] p-0 mt-4">
      <Info className="!w-6 !h-6" />
    </Button>
  );
};

export default InfoButton;
