import React from "react";
import { Card, CardContent, CardHeader, CardTitle } from "../../ui/card";
import { Button } from "../../ui/button";
import { Info } from "lucide-react";
import { Bookmark } from "/imports/api/bookmarks";
import BookmarkButton from "../../ui/BookmarkButton";

interface GalleryCardProps {
  bookmark: Bookmark;
  onclick?: () => void; // method passed by parent to view repository metrics + scaling
  onInfo?: () => void;
}

export default function GalleryCard({
  bookmark,
  onclick,
 onInfo,
}: GalleryCardProps) {
  return (
    <Card className="bg-[#F1502F] w-[275px] ">
  <CardHeader className="relative">
    <div className="flex justify-between items-start">
      <CardTitle className="text-white">{bookmark.title}</CardTitle>

      <div className="flex space-x-2 absolute right-4 top-4">
        {/* info icon */}
        <button
          type="button"
          onClick={onInfo}
          aria-label="repository info"
          className="text-white/80 hover:text-white focus:outline-none"
        >
          <Info size={18} />
        </button>
        {/* bookmark */}
        <BookmarkButton url={bookmark.url} title={bookmark.title} />
      </div>
    </div>
  </CardHeader>      <CardContent className="flex flex-row-reverse">
        <Button variant="secondary" className="rounded-full" onClick={onclick}>
          {" "}
          View Repository
        </Button>
      </CardContent>
    </Card>
  );
}
