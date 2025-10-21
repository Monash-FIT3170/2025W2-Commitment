import React from "react";
import { Search, BarChart3, Settings } from "lucide-react";

const AnalysingRepositoriesTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
        <div className="flex items-center gap-2 mb-4">
          <Search className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">1.0 Analysing Repositories</h2>
        </div>
        <p className="text-git-text-secondary mb-6">
          Learn how to analyze Git repositories, view metrics, and understand scaling data.
        </p>

        {/* Section 1.1 */}
        <div className="mb-6" id="section-1-1">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <span className="text-git-accent-primary">1.1</span> Entering a Repository to Analyse
          </h3>
          <div className="ml-6 space-y-3 text-git-text-secondary">
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Where to enter</h4>
              <p>Content about where users can enter repository URLs for analysis.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Where to find a git link</h4>
              <p>Instructions on finding repository links from GitHub and GitLab.</p>
            </div>
          </div>
        </div>

        {/* Section 1.2 */}
        <div className="mb-6" id="section-1-2">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <BarChart3 className="h-5 w-5 text-git-accent-primary" />
            <span className="text-git-accent-primary">1.2</span> Viewing Metrics
          </h3>
          <div className="ml-6 space-y-3 text-git-text-secondary">
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Filtering and navigation</h4>
              <p>How to filter repositories and navigate through metrics.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Selecting charts</h4>
              <p>Guide to choosing and customizing chart visualizations.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Changing views</h4>
              <p>How to switch between different metric views and perspectives.</p>
            </div>
          </div>
        </div>

        {/* Section 1.3 */}
        <div id="section-1-3">
          <h3 className="text-xl font-semibold text-git-text-primary mb-3 flex items-center gap-2">
            <Settings className="h-5 w-5 text-git-accent-primary" />
            <span className="text-git-accent-primary">1.3</span> Viewing Scaling
          </h3>
          <div className="ml-6 space-y-4 text-git-text-secondary">
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">How to navigate and read</h4>
              <p>Understanding scaling views and navigation.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Signed-in user features</h4>
              <p>Additional features available for authenticated users.</p>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Setting up Grading Sheet</h4>
              <p className="mb-2">How to configure and download grading sheets:</p>
              <ul className="list-disc list-inside ml-4 space-y-1">
                <li>Required fields and data formats</li>
                <li>Downloading templates</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Setting up Alias Config</h4>
              <p className="mb-2">Configure alias mappings for contributors:</p>
              <ul className="list-disc list-inside ml-4 space-y-1">
                <li>JSON format and structure</li>
                <li>What it means for the application</li>
                <li>How it works</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold text-git-text-primary mb-2">Managing scaling for a repo/user</h4>
              <p>How to manage and customize scaling configurations.</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AnalysingRepositoriesTab;
