import React from 'react'
import { Card, CardContent, CardHeader } from '@/imports/ui/components/ui/card'
import { Button } from '@/imports/ui/components/ui/button';

interface Bookmark{
    id:number;
    repoName:string;
    lastViewed:string;// need to consider best way of storing
    repositoryURL:string;

}


interface GalleryCardProps{
    bookmark:Bookmark;
    onclick: ()=>void; // method passed by parent to view repository metrics + scaling 
}

export default function GalleryCard({bookmark, onclick}:GalleryCardProps) {
  return (
    <Card className='md:w-1/4 sm:w-1/2 xs:w-full ' >
    <CardHeader>
        {bookmark.repoName} 
      </CardHeader>
      <CardContent className="flex flex-row-reverse">
        <Button variant="default" onClick={onclick}> View Repository</Button>
      </CardContent>
      
  </Card>  )
}
