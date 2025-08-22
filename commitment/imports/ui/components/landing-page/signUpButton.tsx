import React from 'react';
import { Button } from '@ui/components/ui/button';
import { cn } from '@ui/lib/utils';

interface SignUpButtonProps {
  className?: string;
}

function SignUpButton({ className }: SignUpButtonProps) {
  return (
    <Button
      className={cn(
        'font-mono w-[100px] h-auto text-white rounded-full  text-center bg-git-int-primary hover:bg-git-int-primary-hover drop-shadow-lg',
        className,
      )}
      asChild
    >
      <a href="/signup">Sign Up</a>
    </Button>
  );
}

export default SignUpButton;
