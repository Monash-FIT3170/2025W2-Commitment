import {
  DropdownMenu, DropdownMenuTrigger, DropdownMenuContent, DropdownMenuItem, DropdownMenuSeparator,
} from '@radix-ui/react-dropdown-menu';
import { ArrowDownUp, ArrowDown, ArrowUp } from 'lucide-react';
import React from 'react';
import { Button } from '@base/button';

type SortKey = 'createdAt' | 'lastViewed' | null
type SortDir = 'asc' | 'desc' | null

interface SortMenuOptions{
    sortKey:SortKey;
    sortDir:SortDir
}

export default function SortMenu({sortKey, sortDir}: SortMenuOptions) {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" size="icon" aria-label="sort">
          <ArrowDownUp
            size={20}
            strokeWidth={2}
            className={
            sortDir === 'asc'
              ? 'rotate-180 transition-transform'
              : 'transition-transform'
          }
          />
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuItem onSelect={(e) => { e.preventDefault(); cycle('lastViewed'); }}>
          Last viewed
          {' '}
          {sortKey === 'lastViewed' && (sortDir === 'desc' ? <ArrowDown className="ml-2 h-4 w-4" /> : sortDir === 'asc' ? <ArrowUp className="ml-2 h-4 w-4" /> : null)}
        </DropdownMenuItem>
        <DropdownMenuSeparator />
        <DropdownMenuItem onSelect={(e) => { e.preventDefault(); cycle('createdAt'); }}>
          Date bookmarked
          {' '}
          {sortKey === 'createdAt' && (sortDir === 'desc' ? <ArrowDown className="ml-2 h-4 w-4" /> : sortDir === 'asc' ? <ArrowUp className="ml-2 h-4 w-4" /> : null)}
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
