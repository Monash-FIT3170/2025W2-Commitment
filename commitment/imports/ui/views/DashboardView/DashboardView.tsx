import React, { useEffect, useMemo, useState } from "react";
import GalleryCard from "@ui/components/widgets/dashboard/GalleryCard";
import RepoRow from "../../components/widgets/dashboard/RepoRow";
import ViewToggle from "../../components/widgets/dashboard/ViewToggle";
import { Search  } from "lucide-react";
import { Funnel, ArrowDownUp, ArrowUp, ArrowDown } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
  DropdownMenuContent,
} from "@ui/components/ui/dropdown";
import { Button } from "@ui/components/ui/button";
import { Bookmark } from "/imports/api/bookmarks";
import { Meteor } from 'meteor/meteor';
import { useTracker } from 'meteor/react-meteor-data';
import { NavBar } from "../../components/landing-page/NavBar";
import { DateRange } from "react-day-picker";
import {  FiltersState, FilterValue } from "../../components/ui/filter";
import BookmarkFilter from "../../components/widgets/dashboard/BookmarkFilter";

// const fake_bookmarks:Bookmark[] = Array.from({ length: 12 }, (_, i) => ({
//   _id: `${i + 1}`,
//   title: `Repository ${i + 1}`,
//   createdAt: new Date("2024-06-20T10:30:00.000Z"),
//   url: "git@github.com:Monash-FIT3170/2025W2-Commitment.git",
//   userID:"1"
// }));

const handleView = () => console.log("view metrics");
const handleInfo = () => console.log("info");

// TODO: Filter using regex
const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
  console.log("search:", e.target.value);

type SortKey = "createdAt" | "lastViewed" | null
type SortDir = "asc" | "desc" | null

const DashboardView: React.FC = () => {
  const [view, setView] = useState<"list" | "gallery">("gallery");
  const [bookmarks, setBookmarks] = useState<Bookmark[]>([]);
  const [sortKey, setSortKey] = useState<SortKey>(null);
  const [sortDir, setSortDir] = useState<SortDir>(null);
  const [menuOpen, setMenuOpen] = useState(false);
  const user = useTracker(() => Meteor.user());
  const userName = user?.profile?.name || user?.username || "User";

  
  const [filters, setFilters] = useState<FiltersState>({
    createdAt: { isUsed: false, value: { from: undefined, to: undefined } },
    lastViewed: { isUsed: false, value: { from: undefined, to: undefined } },
  });

  const updateFilter = (key: string, value: FilterValue) => {
    setFilters((prev) => ({
      ...prev,
      [key]: {
        isUsed: Array.isArray(value) ? value.length > 0 : !!value, // fix so is accurate/is updated in method
        value,
      },
    }))
  };


  const applyFilter = (bm: Bookmark, filters: FiltersState):boolean=>{

    return Object.entries(filters).every(([filterKey, { isUsed, value }]) => {
      if (!isUsed) return true;
  
      switch (filterKey) {
        case "createdAt":
          if (value && typeof value === "object" && "from" in value && "to" in value) {
            const range = value as DateRange;
            if (range.from && range.to && bm.createdAt) {
              return bm.createdAt >= range.from && bm.createdAt <= range.to;
            }
          }
          return true;
  
        case "lastViewed":
          if (value && typeof value === "object" && "from" in value && "to" in value) {
            const range = value as DateRange;
            if (range.from && range.to && bm.lastViewed) {
              return bm.lastViewed >= range.from && bm.lastViewed <= range.to;
            }
          }
          return true;
    
        default:
          return true;
      }
    });}

  const applySort = (list: Bookmark[]) => {
    if (!sortKey || !sortDir) return list;
    return [...list].sort((a,b)=>{
      const t1 = new Date((a as any)[sortKey]).getTime();
      const t2 = new Date((b as any)[sortKey]).getTime();
      return sortDir === "asc" ? t1 - t2 : t2 - t1;
    });
  };

  const cycle = (key:"createdAt"|"lastViewed")=>{
    if(sortKey!==key){setSortKey(key);setSortDir("desc");return;}
    if(sortDir==="desc"){setSortDir("asc");return;}
    setSortKey(null);setSortDir(null);
  }

  useEffect(() => {
    Meteor.call("bookmarks.getAllBookmarks", (error, result) => {
      if (error) {
        console.error("Error fetching bookmarks:", error);
      } else {
        setBookmarks(result);
      }
    });

  }, []);

  const displayed = useMemo(()=>{
    const filtered = bookmarks.filter(bm=>applyFilter(bm,filters));
    return applySort(filtered);
  },[bookmarks,filters,sortKey,sortDir]);

  return (
    <div className="min-h-screen bg-[#F0F0E8]">
                  <NavBar isLoggedIn={true} />
      
      {/* Heading (aligned with container left edge) */}
      <h1 className="pt-12 pl-[12%] text-4xl font-bold">{userName}'s Dashboard</h1>

      {/* Toolbar – right-aligned row */}
      <div className="flex justify-end pr-[12%] mt-6 gap-5 items-center flex-wrap">
        <ViewToggle value={view} onChange={setView} className="shrink-0" />

        {/* filter icon */}
        <BookmarkFilter filters={filters} onFilterChange={updateFilter}/>

        {/* sort icon */}
        <DropdownMenu open={menuOpen} onOpenChange={setMenuOpen}>
          <DropdownMenuTrigger asChild>
            <Button variant="outline" size="icon" aria-label="sort">
              <ArrowDownUp
                size={20}
                className={sortDir==="asc"?"rotate-180 transition-transform":"transition-transform"}
              />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent>
            <DropdownMenuItem onSelect={(e)=>{e.preventDefault();cycle("lastViewed");}}>
              Last viewed {sortKey==="lastViewed" && (sortDir==="desc"?<ArrowDown className="ml-2 h-4 w-4"/>:sortDir==="asc"?<ArrowUp className="ml-2 h-4 w-4"/>:null)}
            </DropdownMenuItem>
            <DropdownMenuSeparator />
            <DropdownMenuItem onSelect={(e)=>{e.preventDefault();cycle("createdAt");}}>
              Date bookmarked {sortKey==="createdAt" && (sortDir==="desc"?<ArrowDown className="ml-2 h-4 w-4"/>:sortDir==="asc"?<ArrowUp className="ml-2 h-4 w-4"/>:null)}
            </DropdownMenuItem>
          </DropdownMenuContent>
        </DropdownMenu>

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
          <div className="flex flex-wrap justify-evenly gap-10">
            {displayed.map((b) => (
              <GalleryCard  bookmark={b} onclick={handleView} />
            ))}
          </div>
        ) : (
          // List
          <ul className="space-y-5">
            {displayed.map((b) => (
              <RepoRow
                bookmark={b}
                onclick={handleView}
                onInfo={handleInfo}
              />
            ))}
          </ul>
        )}
      </div>
    </div>
  );
};

export default DashboardView;
