import React, { useEffect, useMemo, useState } from "react";
import GalleryCard from "@ui/components/widgets/dashboard/GalleryCard";
import RepoRow from "../../components/widgets/dashboard/RepoRow";
import ViewToggle from "../../components/widgets/dashboard/ViewToggle";
import { Search, ArrowDownUp } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuTrigger,
  DropdownMenuContent,
  DropdownMenuRadioGroup,
  DropdownMenuRadioItem,
  DropdownMenuSeparator,
} from "@ui/components/ui/dropdown";
import { Button } from "@ui/components/ui/button";
import { Bookmark } from "/imports/api/bookmarks";
import { Meteor } from "meteor/meteor";
import { useTracker } from "meteor/react-meteor-data";
import { NavBar } from "../../components/landing-page/NavBar";
import { DateRange } from "react-day-picker";
import { FiltersState, FilterValue } from "../../components/ui/filter";
import BookmarkFilter from "../../components/widgets/dashboard/BookmarkFilter";

const handleView = () => console.log("view metrics");
const handleInfo = () => console.log("info");

type SortKey = "createdAt" | "lastViewed" | null;
type SortDir = "asc" | "desc";

const sortLabels: Record<string, string> = {
  createdAt_desc: "Date bookmarked: newest → oldest",
  createdAt_asc: "Date bookmarked: oldest → newest",
  lastViewed_desc: "Last accessed: newest → oldest",
  lastViewed_asc: "Last accessed: oldest → newest",
};

const DashboardView: React.FC = () => {
  const [view, setView] = useState<"list" | "gallery">("gallery");
  const [bookmarks, setBookmarks] = useState<Bookmark[]>([]);
  const [sortKey, setSortKey] = useState<SortKey>(null);
  const [sortDir, setSortDir] = useState<SortDir>("desc");
  const user = useTracker(() => Meteor.user());
  const userName = user?.profile?.name || user?.username || "User";

  const [filters, setFilters] = useState<FiltersState>({
    createdAt: { isUsed: false, value: { from: undefined, to: undefined } },
    lastViewed: { isUsed: false, value: { from: undefined, to: undefined } },
    titleSearch: { isUsed: false, value: "" },
  });

  const updateFilter = (key: string, value: FilterValue) => {
    setFilters((p) => ({
      ...p,
      [key]: {
        isUsed: Array.isArray(value) ? value.length > 0 : !!value,
        value,
      },
    }));
  };

  const applyFilter = (bm: Bookmark, filters: FiltersState): boolean => {
    return Object.entries(filters).every(([filterKey, { isUsed, value }]) => {
      if (!isUsed) return true;

      switch (filterKey) {
        case "createdAt":
          if (
            value &&
            typeof value === "object" &&
            "from" in value &&
            "to" in value
          ) {
            const range = value as DateRange;
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
            const range = value as DateRange;
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
  };

  const applySort = (list: Bookmark[], key: SortKey, dir: SortDir) => {
    if (!key) return list;
    return [...list].sort((a, b) => {
      const t1 = new Date((a as any)[key] ?? 0).getTime();
      const t2 = new Date((b as any)[key] ?? 0).getTime();
      return dir === "asc" ? t1 - t2 : t2 - t1;
    });
  };

  const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) => {
    updateFilter("titleSearch", e.target.value);
  };

  useEffect(() => {
    Meteor.call("bookmarks.getAllBookmarks", (error, result) => {
      if (error) {
        console.error("Error fetching bookmarks:", error);
      } else {
        setBookmarks(result);
        console.log("got ", bookmarks.length, " bookmarks");
      }
    });
  }, [bookmarks]);

  const visible = useMemo(() => {
    const filtered = bookmarks.filter((bm) => applyFilter(bm, filters));
    return applySort(filtered, sortKey, sortDir);
  }, [bookmarks, filters, sortKey, sortDir]);

  return (
    <div className="min-h-screen bg-[#F0F0E8]">
      <NavBar isLoggedIn />
      <h1 className="pt-12 pl-[12%] text-4xl font-bold">
        {userName}'s Dashboard
      </h1>

      <div className="flex justify-end pr-[12%] mt-6 gap-5 items-center flex-wrap">
        <ViewToggle value={view} onChange={setView} className="shrink-0" />
        <BookmarkFilter filters={filters} onFilterChange={updateFilter} />

        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button
              variant="outline"
              size="icon"
              title={sortKey ? sortLabels[`${sortKey}_${sortDir}`] : "Sort"}
            >
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

          <DropdownMenuContent sideOffset={6}>
            <DropdownMenuRadioGroup
              value={sortKey ? `${sortKey}_${sortDir}` : ""}
              onValueChange={(v) => {
                const [k, d] = v.split("_") as [SortKey, SortDir];
                setSortKey(k);
                setSortDir(d);
              }}
            >
              <DropdownMenuRadioItem
                value="createdAt_desc"
                className="data-[state=checked]:bg-gray-100 data-[state=checked]:font-medium"
              >
                {sortLabels.createdAt_desc}
              </DropdownMenuRadioItem>
              <DropdownMenuRadioItem
                value="createdAt_asc"
                className="data-[state=checked]:bg-gray-100 data-[state=checked]:font-medium"
              >
                {sortLabels.createdAt_asc}
              </DropdownMenuRadioItem>
              <DropdownMenuSeparator />
              <DropdownMenuRadioItem
                value="lastViewed_desc"
                className="data-[state=checked]:bg-gray-100 data-[state=checked]:font-medium"
              >
                {sortLabels.lastViewed_desc}
              </DropdownMenuRadioItem>
              <DropdownMenuRadioItem
                value="lastViewed_asc"
                className="data-[state=checked]:bg-gray-100 data-[state=checked]:font-medium"
              >
                {sortLabels.lastViewed_asc}
              </DropdownMenuRadioItem>
            </DropdownMenuRadioGroup>
          </DropdownMenuContent>
        </DropdownMenu>

        <div className="relative">
          <Search
            size={16}
            className="absolute left-3 top-1/2 -translate-y-1/2 text-gray-500"
          />
          <input
            type="search"
            placeholder="Search repositories"
            onChange={handleSearch}
            className="w-56 sm:w-64 lg:w-72 rounded-full border border-gray-300 bg-white py-1.5 pl-9 pr-3 text-sm focus:outline-none focus:ring-1 focus:ring-gray-400"
          />
        </div>
      </div>

      <div className="mx-auto mt-6 w-full max-w-[77%] bg-white border border-gray-200 rounded-lg shadow px-6 py-5 h-[480px] overflow-y-auto">
        {view === "gallery" ? (
          <div className="flex flex-wrap justify-evenly gap-10">
            {visible.map((b) => (
              <GalleryCard key={b._id} bookmark={b} onclick={handleView} />
            ))}
          </div>
        ) : (
          <ul className="space-y-5">
            {visible.map((b) => (
              <RepoRow
                key={b._id}
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
