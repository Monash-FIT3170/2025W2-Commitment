import React, { useState } from "react";
import GalleryCard from "@ui/components/widgets/dashboard/GalleryCard";
import RepoRow     from "../../components/widgets/dashboard/RepoRow";
import ViewToggle  from "../../components/widgets/dashboard/ViewToggle";
import { Search }  from "lucide-react";
import { Funnel, ArrowDownUp } from "lucide-react";

const fake_bookmarks = Array.from({ length: 12 }, (_, i) => ({
  id: i + 1,
  repoName: `Repository ${i + 1}`,
  lastViewed: "2024-06-20T10:30:00.000Z",
  repositoryURL: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
}));

const handleView = () => console.log("view metrics");
const handleInfo = () => console.log("info");
const handleBookmark = () => console.log("bookmark");
const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
  console.log("search:", e.target.value);

const DashboardView: React.FC = () => {
  const [view, setView] = useState<"list" | "gallery">("list");

  return (
    <div className="min-h-screen bg-[#F0F0E8]">
      {/* Heading (aligned with container left edge) */}
      <h1 className="pt-12 pl-[12%] text-4xl font-bold">Brian's Dashboard</h1>

      {/* Toolbar – right-aligned row */}
      <div className="flex justify-end pr-[12%] mt-6 gap-5 items-center flex-wrap">
        <ViewToggle value={view} onChange={setView} className="shrink-0" />

        {/* filter icon */}
        <button aria-label="funnel"
                className="p-1.5 rounded hover:bg-gray-100">
          <Funnel size={28} strokeWidth={3} />
        </button>

        {/* sort icon */}
        <button aria-label="sort"
                className="p-1.5 rounded hover:bg-gray-100">
          <ArrowDownUp size={28} strokeWidth={3} />
        </button>

        {/* Search bar */}
        <div className="relative">
          <Search size={16} className="absolute left-3 top-1/2 -translate-y-1/2 text-gray-500" />
          <input
            type="search"
            placeholder="Search repositories"
            onChange={handleSearch}
            className="w-56 sm:w-64 lg:w-72 rounded-full border border-gray-300 bg-white py-1.5 pl-9 pr-3 text-sm focus:outline-none focus:ring-1 focus:ring-gray-400"
          />
        </div>
      </div>

      {/* Outer scrollable container */}
      <div
        className="
          mx-auto mt-6
          w-full max-w-[77%]          
          bg-white border border-gray-200 rounded-lg shadow
          px-6 py-5
          h-[480px]                  
          overflow-y-auto
        "
      >
        {view === "gallery" ? (
          // Gallery
          <div className="flex flex-wrap justify-center gap-14">
            {fake_bookmarks.map((b) => (
              <GalleryCard key={b.id} bookmark={b} onclick={handleView} />
            ))}
          </div>
        ) : (
          // List
          <ul className="space-y-5">
            {fake_bookmarks.map((b) => (
              <RepoRow
                key={b.id}
                name={b.repoName}
                onClick={handleView}
                onInfo={handleInfo}
                onBookmark={handleBookmark}
              />
            ))}
          </ul>
        )}
      </div>
    </div>
  );
};

export default DashboardView;
