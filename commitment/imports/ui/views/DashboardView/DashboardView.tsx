import React from "react";
import GalleryCard from "@ui/components/widgets/dashboard/GalleryCard";

const fake_bookmarks = [
  {
    id: 1,
    repoName: "repository 1",
    lastViewed: "2024-06-20T10:30:00.000Z",
    repositoryURL: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
  },  {
    id: 2,
    repoName: "repository 2",
    lastViewed: "2024-06-20T10:30:00.000Z",
    repositoryURL: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
  },  {
    id: 3,
    repoName: "repository 3",
    lastViewed: "2024-06-20T10:30:00.000Z",
    repositoryURL: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
  },
];

const handleCardClick = () => {
  console.log("navigate to metrics here");
};

const DashboardView = () => (
  <div className="m-9">
    <h1 className="text-cyan-700 font-sans text-5xl m-9 text-center font-thin">
      Dashboard page
    </h1>

    <div className="flex flex-row flex-wrap justify-center gap-5 w-70vw p-5 border-black bg-slate-600 rounded-lg ">
      {fake_bookmarks.map((bookmark) => (
        <GalleryCard bookmark={bookmark} onclick={handleCardClick} />
      ))}
    </div>
  </div>
);

export default DashboardView;
