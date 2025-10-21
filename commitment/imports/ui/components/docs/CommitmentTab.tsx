import React, { useEffect, useState } from "react";
import {
  ExternalLink,
  Github,
  Star,
  GitFork,
  Calendar,
  Code,
  FileText,
  BookOpen,
} from "lucide-react";
import { Button } from "@base/button";

interface GitHubRepoInfo {
  name: string;
  description: string;
  stargazers_count: number;
  forks_count: number;
  language: string;
  created_at: string;
  updated_at: string;
  html_url: string;
  clone_url: string;
  topics: string[];
}

const REPO_OWNER = "Monash-FIT3170";
const REPO_NAME = "2025W2-Commitment";

const CommitmentTab: React.FC = () => {
  const [repoInfo, setRepoInfo] = useState<GitHubRepoInfo | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchRepoInfo = async () => {
      try {
        const response = await fetch(`https://api.github.com/repos/${REPO_OWNER}/${REPO_NAME}`);
        if (!response.ok) {
          throw new Error(`GitHub API error: ${response.status}`);
        }
        const data = await response.json();
        setRepoInfo(data);
      } catch (err) {
        setError(err instanceof Error ? err.message : "Failed to fetch repository info");
      } finally {
        setLoading(false);
      }
    };

    fetchRepoInfo();
  }, []);

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString("en-US", {
      year: "numeric",
      month: "long",
      day: "numeric",
    });
  };

  return (
    <div className="space-y-6">
      <div
        id="commitment-overview"
        className="rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="mb-4 flex items-center gap-2">
          <BookOpen className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">Project Overview</h2>
        </div>

        {loading && (
          <div className="py-8 text-center">
            <div className="mx-auto mb-4 h-8 w-8 animate-spin rounded-full border-b-2 border-git-accent-primary" />
            <p className="text-git-text-secondary">Loading repository information...</p>
          </div>
        )}

        {error && (
          <div className="rounded-lg border border-red-200 bg-red-50 p-4">
            <div className="flex items-center gap-2">
              <FileText className="h-4 w-4 text-red-600" />
              <p className="text-sm text-red-600">Error fetching repository info: {error}</p>
            </div>
          </div>
        )}

        {repoInfo && (
          <div className="space-y-4">
            <div className="flex items-start justify-between gap-4">
              <div className="flex-1">
                <h3 className="mb-2 text-xl font-semibold text-git-text-primary">
                  {repoInfo.name}
                </h3>
                <p className="text-git-text-secondary">
                  {repoInfo.description || "No description available"}
                </p>
              </div>
              <Button
                asChild
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
              >
                <a
                  href={repoInfo.html_url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="flex items-center gap-2"
                >
                  <Github className="h-4 w-4" />
                  View on GitHub
                  <ExternalLink className="h-3 w-3" />
                </a>
              </Button>
            </div>

            <div className="grid grid-cols-2 gap-4 md:grid-cols-4">
              <div className="flex items-center gap-2 text-git-text-secondary">
                <Star className="h-4 w-4 text-yellow-500" />
                <span>{repoInfo.stargazers_count} stars</span>
              </div>
              <div className="flex items-center gap-2 text-git-text-secondary">
                <GitFork className="h-4 w-4 text-blue-500" />
                <span>{repoInfo.forks_count} forks</span>
              </div>
              <div className="flex items-center gap-2 text-git-text-secondary">
                <Code className="h-4 w-4 text-green-500" />
                <span>{repoInfo.language}</span>
              </div>
              <div className="flex items-center gap-2 text-git-text-secondary">
                <Calendar className="h-4 w-4 text-purple-500" />
                <span>Updated {formatDate(repoInfo.updated_at)}</span>
              </div>
            </div>

            {repoInfo.topics && repoInfo.topics.length > 0 && (
              <div>
                <h4 className="mb-2 text-sm font-semibold text-git-text-secondary">Topics</h4>
                <div className="flex flex-wrap gap-2">
                  {repoInfo.topics.map((topic) => (
                    <span
                      key={topic}
                      className="rounded border border-git-stroke-secondary bg-git-bg-secondary px-2 py-1 text-sm text-git-text-primary"
                    >
                      {topic}
                    </span>
                  ))}
                </div>
              </div>
            )}
          </div>
        )}
      </div>

      <div
        id="commitment-links"
        className="rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="mb-4 flex items-center gap-2">
          <FileText className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">Quick Links</h2>
        </div>
        <div className="grid gap-4 md:grid-cols-2">
          <a
            href={`https://github.com/${REPO_OWNER}/${REPO_NAME}`}
            target="_blank"
            rel="noopener noreferrer"
            className="flex items-center gap-3 rounded-lg border border-git-stroke-secondary bg-git-bg-primary p-4 transition-colors hover:bg-git-bg-secondary"
          >
            <Github className="h-5 w-5 text-git-accent-primary" />
            <div className="flex-1">
              <div className="font-semibold text-git-text-primary">Repository</div>
              <div className="text-sm text-git-text-secondary">View source code and issues</div>
            </div>
            <ExternalLink className="h-4 w-4 text-git-text-secondary" />
          </a>

          <a
            href={`https://github.com/${REPO_OWNER}/${REPO_NAME}/issues`}
            target="_blank"
            rel="noopener noreferrer"
            className="flex items-center gap-3 rounded-lg border border-git-stroke-secondary bg-git-bg-primary p-4 transition-colors hover:bg-git-bg-secondary"
          >
            <FileText className="h-5 w-5 text-git-accent-primary" />
            <div className="flex-1">
              <div className="font-semibold text-git-text-primary">Issues & Discussions</div>
              <div className="text-sm text-git-text-secondary">Report bugs and ask questions</div>
            </div>
            <ExternalLink className="h-4 w-4 text-git-text-secondary" />
          </a>
        </div>
      </div>
    </div>
  );
};

export default CommitmentTab;
