import React from "react";
import { Search } from "lucide-react";

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

const AnalysingRepositoriesTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <section
        id="section-1-1"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div>
          <div className="mb-4 flex items-center gap-3">
            <Search className="h-6 w-6 text-git-accent-primary" />
            <h2 className="text-2xl font-semibold text-git-text-primary">1.0 Analysing Repositories</h2>
          </div>
          <p className="text-git-text-secondary">
            Learn how to analyze Git repositories, view metrics, and understand scaling data.
          </p>
        </div>

        <div className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">1.1 Entering a Repository to Analyse</h3>
          <p className="text-git-text-secondary">
            On Commitment&rsquo;s home page, enter the link of the git repository you wish to analyse and
            click the <span className="font-semibold text-git-text-primary">Analyse Repository</span>
            {" "}button to begin.
          </p>
          {imageCard(
            "/docs_images/analysingrepos/Picture2.png",
            "Commitment home page showing the Analyse Repository input and button."
          )}
          <p className="text-git-text-secondary">
            This link can be obtained via your repository host&rsquo;s website:
          </p>
          {imageCard(
            "/docs_images/analysingrepos/Picture3.png",
            "Location of the cloning link in GitHub."
          )}
          <p className="text-sm text-git-text-secondary">Location of cloning link in GitHub</p>
          {imageCard(
            "/docs_images/analysingrepos/Picture4.png",
            "Location of the cloning link in GitLab."
          )}
          <p className="text-sm text-git-text-secondary">Location of cloning link in GitLab</p>
        </div>
      </section>

      <section
        id="section-1-2"
        className="space-y-5 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h2 className="text-2xl font-semibold text-git-text-primary">1.2 Viewing Metrics</h2>
        <p className="text-git-text-secondary">
          The metrics page provides an overview of the work completed by individual contributors.
        </p>
        {imageCard(
          "/docs_images/analysingrepos/Picture5.png",
          "Commitment metrics page overview."
        )}
        <p className="text-sm text-git-text-secondary">Metrics page</p>
        <p className="text-git-text-secondary">
          There are various filters available to adjust the data on display. After modifying these
          parameters, select the <span className="font-semibold text-git-text-primary">Apply</span>{" "}
          button to use your selected filters:
        </p>
        {imageCard(
          "/docs_images/analysingrepos/Picture6.png",
          "Filter controls available on the metrics page."
        )}
        <p className="text-sm text-git-text-secondary">Available filters for the metrics page</p>
        <div className="space-y-4 text-git-text-secondary">
          <div className="space-y-3">
            <h3 className="text-lg font-semibold text-git-text-primary">Date range</h3>
            <p>
              Allows users to set the date range of metrics to be considered. Preset ranges are available,
              and custom ranges can be selected using the mini calendars.
            </p>
            {imageCard(
              "/docs_images/analysingrepos/Picture7.png",
              "Date range picker on the metrics page."
            )}
          </div>
          <div className="space-y-3">
            <h3 className="text-lg font-semibold text-git-text-primary">Branch</h3>
            <p>
              Users may select a specific branch to analyse commits from. By default, this is the main branch.
            </p>
            <div className="rounded-md border border-git-stroke-secondary bg-git-bg-secondary px-4 py-3 text-sm text-git-text-secondary">
              NOTE: Functionality to analyse all commits across all branches is not currently a feature of Commitment.
            </div>
            {imageCard(
              "/docs_images/analysingrepos/Picture8.png",
              "Branch selector control."
            )}
          </div>
          <div className="space-y-3">
            <h3 className="text-lg font-semibold text-git-text-primary">Contributors</h3>
            <p>
              Allows the selection of specific developers to analyse. By default, metrics include all committers.
            </p>
            {imageCard(
              "/docs_images/analysingrepos/Picture9.png",
              "Contributors multi-select control."
            )}
          </div>
          <div className="space-y-3">
            <h3 className="text-lg font-semibold text-git-text-primary">Metrics</h3>
            <p>Switch between different metrics to tailor the insights shown on the page.</p>
            {imageCard(
              "/docs_images/analysingrepos/Picture10.png",
              "Metrics selection dropdown."
            )}
          </div>
        </div>
      </section>

      <section
        id="section-1-3"
        className="space-y-5 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h2 className="text-2xl font-semibold text-git-text-primary">1.3 Viewing Scaling</h2>
        <p className="text-git-text-secondary">
          The scaling page surfaces per-committer activity summaries alongside their calculated scaling scores.
        </p>
        {imageCard(
          "/docs_images/analysingrepos/Picture11.png",
          "Scaling page overview with contributor entries."
        )}
        <p className="text-sm text-git-text-secondary">Scaling page</p>
        <p className="text-git-text-secondary">
          For each committer you can review their associated accounts as well as their scaling score.
        </p>
        {imageCard(
          "/docs_images/analysingrepos/Picture12.png",
          "Scaling card showing contributor accounts and scaling score."
        )}
        <div id="section-1-3-2" className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">1.3.2 Generating Scaling</h3>
          <p className="text-git-text-secondary">
            When first opening the scaling page for a repository, you are prompted to select the metrics that influence scaling, along with the desired method for generating the results.
          </p>
          {imageCard(
            "/docs_images/analysingrepos/Picture13.png",
            "Generate Scalings modal with metric selections."
          )}
          <p className="text-sm text-git-text-secondary">Generate Scalings modal</p>
          <p className="text-git-text-secondary">
            After completing the setup, select the <span className="font-semibold text-git-text-primary">Next</span> button to generate the scalings. If you wish to regenerate using a different method, use the <span className="font-semibold text-git-text-primary">Create New Scaling</span> button at the bottom of the page.
          </p>
          {imageCard(
            "/docs_images/analysingrepos/Picture14.png",
            "Create new scaling button on the scaling page."
          )}
        </div>
      </section>
    </div>
  );
};

export default AnalysingRepositoriesTab;
