import React from 'react';
import { format } from 'date-fns';
import {
  Card, CardContent, CardHeader, CardTitle,
} from '../ui/card';
import { Button } from '../ui/button';
import { Bookmark } from '/imports/api/bookmarks';
import BookmarkButton from '../ui/BookmarkButton';

interface GalleryCardProps {
  bookmark: Bookmark;
  onclick?: () => void; // method passed by parent to view repository metrics + scaling
}

export default function GalleryCard({
  bookmark,
  onclick,
}: GalleryCardProps) {
  const descriptionText = `Created on : ${format(
    bookmark.createdAt,
    'dd MMM yyyy',
  )}\nLast viewed: ${
    bookmark.lastViewed
      ? format(bookmark.lastViewed, 'dd MMM yyyy')
      : format(bookmark.createdAt, 'dd MMM yyyy')
  }`;

  return (
    <Card className="bg-[#F1502F] w-[275px] ">
      <CardHeader className="relative">
        <div className="flex justify-between items-start">
          <CardTitle className="text-white">{bookmark.title}</CardTitle>

          <div className="flex space-x-2 absolute right-4 top-4">
            {/* info icon */}

            {/* <InfoButton description/> */}

            {/* bookmark */}
            <BookmarkButton url={bookmark.url} title={bookmark.title} />
          </div>
        </div>
      </CardHeader>
      {' '}
      <CardContent className="flex flex-row-reverse">
        <Button variant="secondary" className="rounded-full" onClick={onclick}>
          {' '}
          View Repository
        </Button>
      </CardContent>
    </Card>
  );
}
