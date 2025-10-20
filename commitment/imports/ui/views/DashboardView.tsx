import React, { useEffect, useMemo, useState } from "react";
import GalleryCard from "/imports/ui/components/dashboard/GalleryCard";
import RepoCard from "../components/home/RepoCard";
import { Search, ArrowDownUp, ArrowUp, ArrowDown } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
  DropdownMenuContent,
} from "@base/dropdown-menu";
import { Button } from "@base/button";
import { Bookmark } from "@api/bookmarks";
import { Meteor } from "meteor/meteor";
import { useTracker } from "meteor/react-meteor-data";
import { useNavigate } from "react-router-dom";
import ViewToggle from "../components/dashboard/ViewToggle";
import RepoRow from "../components/dashboard/RepoRow";
import { FiltersState, FilterValue } from "@base/filter";
import BookmarkFilter from "../components/dashboard/BookmarkFilter";
import { Input } from "@base/input";

/*
 const fake_bookmarks:Bookmark[] = Array.from({ length: 5 }, (_, i) => ({
   _id: `${i + 1}`,
   title: `Repository ${i + 1}`,
   createdAt: new Date("2024-06-20T10:30:00.000Z"),
   url: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
   userID:"1"
  }));
*/

type SortKey = "createdAt" | "lastViewed" | null;
type SortDir = "asc" | "desc" | null;

const DashboardView: React.FC = () => {
  const [view, setView] = useState<"list" | "gallery">("gallery");
  const [bookmarks, setBookmarks] = useState<Bookmark[]>([]);
  const [sortKey, setSortKey] = useState<SortKey>(null);
  const [sortDir, setSortDir] = useState<SortDir>(null);
  const user = useTracker(() => Meteor.user());
  const userName = user?.profile?.name || user?.username || "User";
  const navigate = useNavigate();

  // Clear repository history when user navigates to dashboard
  useEffect(() => {
    localStorage.removeItem('lastRepoUrl');
  }, []);

  const [filters, setFilters] = useState<FiltersState>({
    createdAt: { isUsed: false, value: { from: undefined, to: undefined } },
    lastViewed: { isUsed: false, value: { from: undefined, to: undefined } },
    titleSearch: { isUsed: false, value: "" },
  });

  const updateFilter = (key: string, value: FilterValue) => {
    setFilters((prev) => ({
      ...prev,
      [key]: {
        isUsed: Array.isArray(value) ? value.length > 0 : !!value, // fix so is accurate/is updated in method
        value,
      },
    }));
  };

  const applyFilter = (bm: Bookmark, filterState: FiltersState): boolean =>
    Object.entries(filterState).every(([filterKey, { isUsed, value }]) => {
      if (!isUsed) return true;

      switch (filterKey) {
        case "createdAt":
          if (
            value &&
            typeof value === "object" &&
            "from" in value &&
            "to" in value
          ) {
            const range = value;
            if (range.from && range.to && bm.createdAt) {
              return bm.createdAt >= range.from && bm.createdAt <= range.to;
            }
          }
          return true;

        case "lastViewed":
          if (
            value &&
            typeof value === "object" &&
            "from" in value &&
            "to" in value
          ) {
            const range = value;
            if (range.from && range.to && bm.lastViewed) {
              return bm.lastViewed >= range.from && bm.lastViewed <= range.to;
            }
          }
          return true;

        case "titleSearch":
          console.log("searching w/ string!");
          if (value && typeof value === "string") {
            return bm.title.toLowerCase().includes(value.toLowerCase());
          }
          return true;

        default:
          return true;
      }
    });

  const applySort = (bookmarks: Bookmark[]) => {
    if (!sortKey || !sortDir) return bookmarks;

    return [...bookmarks].sort((a, b) => {
      let aValue: any = a[sortKey];
      let bValue: any = b[sortKey];

      if (aValue instanceof Date) aValue = aValue.getTime();
      if (bValue instanceof Date) bValue = bValue.getTime();

      if (sortDir === "asc") {
        return aValue > bValue ? 1 : -1;
      } 
        return aValue < bValue ? 1 : -1;
      
    });
  };

  const cycle = (key: "createdAt" | "lastViewed") => {
    if (sortKey !== key) {
      setSortKey(key);
      setSortDir("desc");
      return;
    }
    if (sortDir === "desc") {
      setSortDir("asc");
      return;
    }
    setSortKey(null);
    setSortDir(null);
  };

  const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) => {
    updateFilter("titleSearch", e.target.value);
  };

  const handleViewRepository = (bookmark: Bookmark) => {
    // Update lastViewed timestamp
    Meteor.call("bookmarks.updateLastViewed", bookmark.url, (error: any) => {
      if (error) {
        console.error("Error updating lastViewed:", error);
      }
    });

    navigate("/metrics", {
      state: { repoUrl: bookmark.url }
    });
  };

  
  useEffect(() => {
    Meteor.call(
      "bookmarks.getAllBookmarks",
      (error: any, result: React.SetStateAction<Bookmark[]>) => {
        if (error) {
          console.error("Error fetching bookmarks:", error);
        } else {
          setBookmarks(result);
          console.log("got ", bookmarks.length, " bookmarks");
        }
      }
    );
  }, [bookmarks]);
 
  /*
  useEffect(() => {
    setBookmarks(fake_bookmarks);
  }, []);
  */

  const displayed = useMemo(() => {
    const filtered = bookmarks.filter((bm) => applyFilter(bm, filters));
    return applySort(filtered);
  }, [bookmarks, filters, sortKey, sortDir]);


  return (
    <>
      
      <h1 className="pt-12 pl-[12%] text-4xl font-bold">
        {userName}
        &apos;s Dashboard
      </h1>

      {/* Toolbar – right-aligned row */}
      <div className="flex justify-end pr-[12%] mt-6 gap-5 items-center flex-wrap">
        <ViewToggle value={view} onChange={setView} />

        {/* filter icon */}
        <BookmarkFilter filters={filters} onFilterChange={updateFilter} />

        {/* sort icon */}
        {/* TODO: Move this into it's own component  */}
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline" size="icon" aria-label="sort">
              <ArrowDownUp
                size={20}
                strokeWidth={2}
                className={
                  sortDir === "asc"
                    ? "rotate-180 transition-transform"
                    : "transition-transform"
                }
              />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent>
            <DropdownMenuItem
              onSelect={(e) => {
                e.preventDefault();
                cycle("lastViewed");
              }}
            >
              Last viewed{" "}
              {sortKey === "lastViewed" &&
                (sortDir === "desc" ? (
                  <ArrowDown className="ml-2 h-4 w-4" />
                ) : sortDir === "asc" ? (
                  <ArrowUp className="ml-2 h-4 w-4" />
                ) : null)}
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem
              onSelect={(e) => {
                e.preventDefault();
                cycle("createdAt");
              }}
            >
              Date bookmarked{" "}
              {sortKey === "createdAt" &&
                (sortDir === "desc" ? (
                  <ArrowDown className="ml-2 h-4 w-4" />
                ) : sortDir === "asc" ? (
                  <ArrowUp className="ml-2 h-4 w-4" />
                ) : null)}
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>

        {/* Search bar */}
        <div className="relative">

          <Input
            type="search"
            placeholder="Search repositories"
            onChange={handleSearch}
            className="w-56 pl-4 sm:w-64 lg:w-72 rounded-full border "

          />
        </div>
      </div>

      {/* Outer scrollable container */}
      <div
        className="
        mx-auto mt-6
        w-full max-w-[77%]
        bg-git-bg-secondary/20 dark:bg-git-bg-secondary
        border border-border rounded-lg shadow
        px-6 py-5
        h-[480px]
        overflow-y-auto
      "
      >
        {view === "gallery" ? (
          // Gallery
          <div
            className={
              displayed.length >= 3
                // Many repos: cap at 3 columns on xl+, auto-fit below
                ? "grid gap-6 grid-cols-[repeat(auto-fit,minmax(320px,1fr))] xl:[grid-template-columns:repeat(3,minmax(320px,1fr))]"
                : displayed.length === 2
                  // Exactly two: clean two-up
                  ? "grid gap-6 grid-cols-1 md:grid-cols-2"
                  // Single: center and constrain width so it doesn't look huge
                  : "grid gap-6 grid-cols-1 md:max-w-3xl lg:max-w-5xl xl:max-w-none w-full mx-auto"
            }
          >
            {displayed.map((b) => (
              <RepoCard
                key={b._id}
                repository={b}
                onClick={() => handleViewRepository(b)}
              />
            ))}
          </div>
        ) : (
          // List
          <ul className="space-y-5">
            {displayed.map((b) => (
              <RepoRow key={b._id} bookmark={b} onclick={() => handleViewRepository(b)} />
            ))}
          </ul>
        )}
      </div>
    </>
  );
};

export default DashboardView;
