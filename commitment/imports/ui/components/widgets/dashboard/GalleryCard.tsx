import React from "react";
import { Card, CardContent, CardHeader, CardTitle } from "../../ui/card";
import { Button } from "../../ui/button";
import { Bookmark, Info } from "lucide-react";

interface Bookmark {
  id: number;
  repoName: string;
  lastViewed: string; // need to consider best way of storing
  repositoryURL: string;
}

interface GalleryCardProps {
  bookmark: Bookmark;
  onclick?: () => void; // method passed by parent to view repository metrics + scaling
  onBookmark?: () => void;
  onInfo?: () => void;
}

export default function GalleryCard({
  bookmark,
  onclick,
  onBookmark,
  onInfo,
}: GalleryCardProps) {
  return (
    <Card className="bg-[#F1502F] w-[250px] ">
      <CardHeader className="flex flex-row justify-between items-start">
        <CardTitle className="text-white">{bookmark.repoName} </CardTitle>

        <div className="flex flex-row h-full justify-end ">
          {" "}
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
          <button
            type="button"
            onClick={onBookmark}
            aria-label="bookmark repository"
            className="text-white/80 hover:text-white focus:outline-none"
          >
            <Bookmark size={18} />
          </button>
        </div>
      </CardHeader>
      <CardContent className="flex flex-row-reverse">
        <Button variant="secondary" onClick={onclick}>
          {" "}
          View Repository
        </Button>
      </CardContent>
    </Card>
  );
}
