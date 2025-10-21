import React, { useEffect } from "react";
import {
  Search,
  UserCircle,
  Bookmark,
  Code2,
  FileSpreadsheet,
  FileText,
  BookOpen,
  ChevronRight,
} from "lucide-react";
import { ResizableHandle, ResizablePanel, ResizablePanelGroup } from "@base/resizable";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "@base/collapsible";
import { cn } from "@ui/lib/utils";
import AnalysingRepositoriesTab from "@ui/components/docs/AnalysingRepositoriesTab";
import AccountManagementTab from "@ui/components/docs/AccountManagementTab";
import BookmarksTab from "@ui/components/docs/BookmarksTab";
import CustomScalingTab from "@ui/components/docs/CustomScalingTab";
import GradingSheetTab from "@ui/components/docs/GradingSheetTab";
import RESTAPITab from "@ui/components/docs/RESTAPITab";
import CommitmentTab from "@ui/components/docs/CommitmentTab";

type DocSection =
  | "commitment"
  | "analysing-repositories"
  | "account-management"
  | "bookmarks"
  | "custom-scaling"
  | "grading-sheet"
  | "rest-api";

interface SubSection {
  id: string;
  label: string;
  number: string;
}

interface SidebarItem {
  id: DocSection;
  label: string;
  icon: React.ElementType;
  number: string;
  subSections?: SubSection[];
}

const sidebarItems: SidebarItem[] = [
  {
    id: "commitment",
    label: "Commitment Overview",
    icon: BookOpen,
    number: "0.0",
    subSections: [
      { id: "commitment-overview", label: "Project Overview", number: "0.1" },
      { id: "commitment-links", label: "Quick Links", number: "0.2" },
    ],
  },
  {
    id: "analysing-repositories",
    label: "Analysing Repositories",
    icon: Search,
    number: "1.0",
    subSections: [
      { id: "section-1-1", label: "Entering a Repository to Analyse", number: "1.1" },
      { id: "section-1-2", label: "Viewing Metrics", number: "1.2" },
      { id: "section-1-3", label: "Viewing Scaling", number: "1.3" },
    ],
  },
  {
    id: "account-management",
    label: "Account Management",
    icon: UserCircle,
    number: "2.0",
    subSections: [
      { id: "section-2-1", label: "Account Creation", number: "2.1" },
      { id: "section-2-2", label: "Login", number: "2.2" },
      { id: "section-2-3", label: "Personalisation", number: "2.3" },
    ],
  },
  {
    id: "bookmarks",
    label: "Bookmarks & Dashboard",
    icon: Bookmark,
    number: "3.0",
  },
  {
    id: "custom-scaling",
    label: "Custom Scaling",
    icon: Code2,
    number: "4.0",
  },
  {
    id: "grading-sheet",
    label: "Grading Sheet Upload",
    icon: FileSpreadsheet,
    number: "5.0",
  },
  {
    id: "rest-api",
    label: "REST API",
    icon: FileText,
    number: "6.0",
    subSections: [
      { id: "rest-api-calling", label: "Calling the API", number: "6.1" },
      { id: "rest-api-usage", label: "How We Use It", number: "6.2" },
      { id: "rest-api-further", label: "Further Queries", number: "6.3" },
    ],
  },
];

interface DocsResizableLayoutProps {
  items: SidebarItem[];
  activeSection: DocSection;
  onSelectSection: (section: DocSection) => void;
  openSections: Set<DocSection>;
  onToggleSection: (section: DocSection) => void;
  children: React.ReactNode;
}

const DocsResizableLayout: React.FC<DocsResizableLayoutProps> = ({
  items,
  activeSection,
  onSelectSection,
  openSections,
  onToggleSection,
  children,
}) => {
  const [isCompact, setIsCompact] = React.useState(false);
  const contentRef = React.useRef<HTMLDivElement>(null);

  const handleResize = React.useCallback((size: number) => {
    setIsCompact(size <= 10);
  }, []);

  const handleScrollTop = React.useCallback(() => {
    const container = contentRef.current;
    if (!container) {
      return;
    }
    container.scrollTo({ top: 0, behavior: "smooth" });
  }, []);

  const handleScrollTo = React.useCallback((sectionId: string) => {
    const container = contentRef.current;
    if (!container) {
      return;
    }
    const target = container.querySelector<HTMLElement>(`#${sectionId}`);
    if (!target) {
      return;
    }
    const containerRect = container.getBoundingClientRect();
    const targetRect = target.getBoundingClientRect();
    const offset = targetRect.top - containerRect.top + container.scrollTop - 16;
    container.scrollTo({
      top: offset,
      behavior: "smooth",
    });
  }, []);

  return (
    <ResizablePanelGroup
      direction="horizontal"
      className="flex h-full bg-git-bg-primary"
    >
      <ResizablePanel
        defaultSize={24}
        minSize={4}
        maxSize={45}
        onResize={handleResize}
        className="min-w-[44px] max-w-[420px]"
      >
        <div className="relative h-full w-full overflow-hidden border-r border-git-stroke-primary bg-git-bg-secondary text-git-text-primary">
          <div className="flex h-full flex-col">
            <div className="flex-1 overflow-y-auto px-4 py-4">
              <nav className="flex flex-col gap-3">
              {items.map((item) => {
                const Icon = item.icon;
                const hasSubSections = item.subSections && item.subSections.length > 0;
                const isActive = activeSection === item.id;

                if (!hasSubSections) {
                  return (
                    <button
                      key={item.id}
                      type="button"
                      onClick={() => {
                        if (isActive) {
                          handleScrollTop();
                        } else {
                          onSelectSection(item.id);
                        }
                      }}
                        className={cn(
                          "flex w-full items-center gap-3 rounded-md px-3 py-2 text-sm transition-colors",
                          "hover:bg-git-bg-primary/50 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-git-int-primary",
                          isActive
                            ? "bg-git-bg-primary/60 text-git-text-primary"
                            : "text-git-text-secondary",
                          isCompact && "justify-center px-1 py-2 text-[10px]"
                        )}
                    >
                      <Icon className="h-4 w-4 shrink-0 text-git-accent-primary" />
                      <span
                        className={cn(
                          "truncate text-left text-sm",
                          isCompact && "sr-only"
                        )}
                      >
                        {item.label}
                      </span>
                    </button>
                  );
                }

                const isOpen = openSections.has(item.id);

                return (
                  <Collapsible
                    key={item.id}
                    open={isOpen}
                    onOpenChange={() => onToggleSection(item.id)}
                  >
                    <div className="flex items-center gap-2">
                      <button
                        type="button"
                        onClick={() => {
                          if (isActive) {
                            handleScrollTop();
                          } else {
                            onSelectSection(item.id);
                          }
                        }}
                        className={cn(
                          "flex flex-1 items-center gap-3 rounded-md px-3 py-2 text-sm transition-colors",
                          "hover:bg-git-bg-primary/50 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-git-int-primary",
                          isActive
                            ? "bg-git-bg-primary/60 text-git-text-primary"
                            : "text-git-text-secondary",
                          isCompact && "justify-center px-1 py-2 text-[10px]"
                        )}
                      >
                        <Icon className="h-4 w-4 shrink-0 text-git-accent-primary" />
                        <span
                          className={cn(
                            "truncate text-left text-sm",
                            isCompact && "sr-only"
                          )}
                        >
                          {item.label}
                        </span>
                      </button>
                      {!isCompact && (
                        <CollapsibleTrigger asChild>
                          <button
                            type="button"
                            className={cn(
                              "flex h-9 w-9 items-center justify-center rounded-lg text-git-text-secondary transition-colors",
                              "hover:bg-git-bg-primary/40 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-git-int-primary"
                            )}
                            aria-label={isOpen ? `Collapse ${item.label}` : `Expand ${item.label}`}
                          >
                            <ChevronRight
                              className={cn(
                                "h-4 w-4 transition-transform duration-200",
                                isOpen && "rotate-90"
                              )}
                            />
                          </button>
                        </CollapsibleTrigger>
                      )}
                    </div>
                    {!isCompact && (
                      <CollapsibleContent>
                        <div className="ml-5 mt-2 border-l border-git-stroke-secondary pl-4">
                          {item.subSections!.map((subSection) => (
                            <button
                              key={subSection.id}
                              type="button"
                              onClick={() => {
                                if (activeSection !== item.id) {
                                  onSelectSection(item.id);
                                  requestAnimationFrame(() => {
                                    requestAnimationFrame(() => handleScrollTo(subSection.id));
                                  });
                                } else {
                                  handleScrollTo(subSection.id);
                                }
                              }}
                              className={cn(
                                "flex w-full items-center gap-3 rounded-md px-3 py-2 text-sm text-git-text-secondary transition-colors",
                                "hover:bg-git-bg-primary/60 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-git-int-primary"
                              )}
                            >
                              <span className="truncate">{subSection.label}</span>
                            </button>
                          ))}
                        </div>
                      </CollapsibleContent>
                    )}
                  </Collapsible>
                );
                })}
              </nav>
            </div>
          </div>
        </div>
      </ResizablePanel>
      <ResizableHandle withHandle className="bg-git-stroke-primary/40" />
      <ResizablePanel defaultSize={76} minSize={40} className="min-w-[320px]">
        <div className="flex h-full min-h-0 flex-col overflow-hidden bg-git-bg-primary">
          <div className="sticky top-0 z-10 border-b border-git-stroke-secondary bg-git-bg-primary px-6 py-4">
            <h1 className="text-3xl font-bold text-git-text-primary">Commitment User Guides</h1>
          </div>
          <div ref={contentRef} className="flex-1 overflow-y-auto px-6 pb-6">
            <div className="flex flex-col gap-4 pt-4">{children}</div>
          </div>
        </div>
      </ResizablePanel>
    </ResizablePanelGroup>
  );
};

const DocsView: React.FC = () => {
  const [activeSection, setActiveSection] = React.useState<DocSection>("commitment");
  const [openSections, setOpenSections] = React.useState<Set<DocSection>>(
    new Set(["commitment", "analysing-repositories"])
  );

  // Clear repository history when user navigates to docs page
  useEffect(() => {
    localStorage.removeItem('lastRepoUrl');
  }, []);

  const toggleSection = (sectionId: DocSection) => {
    setOpenSections(prev => {
      const newSet = new Set(prev);
      if (newSet.has(sectionId)) {
        newSet.delete(sectionId);
      } else {
        newSet.add(sectionId);
      }
      return newSet;
    });
  };

  const renderContent = () => {
    switch (activeSection) {
      case "commitment":
        return <CommitmentTab />;
      case "analysing-repositories":
        return <AnalysingRepositoriesTab />;
      case "account-management":
        return <AccountManagementTab />;
      case "bookmarks":
        return <BookmarksTab />;
      case "custom-scaling":
        return <CustomScalingTab />;
      case "grading-sheet":
        return <GradingSheetTab />;
      case "rest-api":
        return <RESTAPITab />;
      default:
        return <CommitmentTab />;
    }
  };

  return (
    <div className="h-[calc(100vh-3.5rem)] bg-git-bg-primary">
      <DocsResizableLayout
        items={sidebarItems}
        activeSection={activeSection}
        onSelectSection={setActiveSection}
        openSections={openSections}
        onToggleSection={toggleSection}
      >
        {renderContent()}
      </DocsResizableLayout>
    </div>
  );
};

export default DocsView;
