import React from "react";
import { Bookmark } from "lucide-react";

const imageCard = (src: string, alt: string, caption?: string): React.ReactElement => (
  <figure className="overflow-hidden rounded-lg border border-git-stroke-primary bg-git-bg-elevated shadow-lg">
    <img src={src} alt={alt} className="h-auto w-full rounded-lg" />
    {caption && (
      <figcaption className="border-t border-git-stroke-secondary bg-git-bg-secondary px-4 py-2 text-sm text-git-text-secondary">
        {caption}
      </figcaption>
    )}
  </figure>
);

const BookmarksTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <section
        id="section-3-1"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="space-y-3">
          <div className="flex items-center gap-3">
            <Bookmark className="h-6 w-6 text-git-accent-primary" />
            <h2 className="text-2xl font-semibold text-git-text-primary">3.0 Bookmarks and the Dashboard</h2>
          </div>
          <p className="text-git-text-secondary">
            Bookmarks let you jump straight back to repositories you are actively analysing, and the dashboard
            surfaces those saved projects for quick access.
          </p>
        </div>

        <div className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">3.1 Bookmarking a Repository</h3>
          <p className="text-git-text-secondary">
            Bookmarking a repository is a convenient way to keep important projects one click away from the
            dashboard.
          </p>
          <p className="text-git-text-secondary">
            After submitting a GitHub link for analysis, the metrics page displays the repository details and a
            bookmark control.
          </p>
          {imageCard(
            "/docs_images/bookmarks/Picture23.png",
            "Metrics page showing the analysed repository."
          )}
          <p className="text-sm text-git-text-secondary">Metrics page</p>
          <p className="text-git-text-secondary">
            To bookmark the repository, press the bookmark icon next to the repository name.
          </p>
          {imageCard(
            "/docs_images/bookmarks/Picture24.png",
            "Bookmark button beside the repository title."
          )}
          <div className="text-git-text-secondary">
            <img
              src="/docs_images/bookmarks/Picture25.png"
              alt="Bookmark button filled to indicate success."
              className="float-right ml-4 w-32 rounded-md border border-git-stroke-primary bg-git-bg-elevated"
            />
            <p>
              A successful bookmark is indicated when the button appears filled, confirming the repository is now
              saved.
            </p>
            <div className="clear-right" />
          </div>
        </div>
      </section>

      <section
        id="section-3-2"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">3.2 Unbookmarking a Repository</h3>
        <p className="text-git-text-secondary">
          Unbookmarking follows the same steps. Click the bookmark icon again and the repository is removed from
          your saved list.
        </p>
        {imageCard(
          "/docs_images/bookmarks/Picture26.png",
          "Metrics page showing a bookmarked repository ready to be removed."
        )}
        <p className="text-sm text-git-text-secondary">Metrics page with bookmarked repository</p>
      </section>
    </div>
  );
};

export default BookmarksTab;
