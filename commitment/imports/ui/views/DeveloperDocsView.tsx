import React, { useEffect, useState } from "react";
import { ExternalLink, Github, Star, GitFork, Calendar, Code, Users, FileText, BookOpen } from "lucide-react";
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

const DeveloperDocsView: React.FC = () => {
  const [repoInfo, setRepoInfo] = useState<GitHubRepoInfo | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const REPO_OWNER = "Monash-FIT3170";
  const REPO_NAME = "2025W2-Commitment";

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
      day: "numeric"
    });
  };

  return (
    <div className="min-h-screen bg-git-bg-primary">
      <div className="max-w-6xl mx-auto px-6 py-8">
        <div className="mb-6">
          <h1 className="text-4xl font-bold text-git-text-primary">
            Developer Docs
          </h1>
          <p className="text-git-text-secondary text-lg mt-2">
            Documentation for using our REST APIs and useful information about the Commitment project.
          </p>
        </div>

        <div className="space-y-6">
          {/* Project Overview Section */}
          <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
            <div className="flex items-center gap-2 mb-4">
              <Github className="h-6 w-6 text-git-accent-primary" />
              <h2 className="text-2xl font-semibold text-git-text-primary">Project Overview</h2>
            </div>
            
            {loading && (
              <div className="text-center py-8">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-git-accent-primary mx-auto mb-4" />
                <p className="text-git-text-secondary">Loading repository information...</p>
              </div>
            )}
            
            {error && (
              <div className="p-4 bg-red-50 border border-red-200 rounded-lg">
                <div className="flex items-center gap-2">
                  <FileText className="h-4 w-4 text-red-600" />
                  <p className="text-red-600 text-sm">Error fetching repository info: {error}</p>
                </div>
              </div>
            )}
            
            {repoInfo && (
              <div className="space-y-4">
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <h3 className="text-xl font-semibold text-git-text-primary mb-2">{repoInfo.name}</h3>
                    <p className="text-git-text-secondary mb-4">{repoInfo.description || "No description available"}</p>
                  </div>
                  <Button
                    asChild
                    className="bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text"
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

                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
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
                    <h4 className="text-sm font-semibold text-git-text-secondary mb-2">Topics</h4>
                    <div className="flex flex-wrap gap-2">
                      {repoInfo.topics.map((topic) => (
                        <span
                          key={topic}
                          className="bg-git-bg-secondary text-git-text-primary px-2 py-1 rounded text-sm border border-git-stroke-secondary"
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

          {/* Quick Links Section */}
          <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
            <div className="flex items-center gap-2 mb-4">
              <BookOpen className="h-6 w-6 text-git-accent-primary" />
              <h2 className="text-2xl font-semibold text-git-text-primary">Quick Links</h2>
            </div>
            <div className="grid md:grid-cols-2 gap-4">
              <a
                href={`https://github.com/${REPO_OWNER}/${REPO_NAME}`}
                target="_blank"
                rel="noopener noreferrer"
                className="flex items-center gap-3 p-4 bg-git-bg-primary hover:bg-git-bg-secondary rounded-lg transition-colors border border-git-stroke-secondary"
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
                className="flex items-center gap-3 p-4 bg-git-bg-primary hover:bg-git-bg-secondary rounded-lg transition-colors border border-git-stroke-secondary"
              >
                <Users className="h-5 w-5 text-git-accent-primary" />
                <div className="flex-1">
                  <div className="font-semibold text-git-text-primary">Issues & Discussions</div>
                  <div className="text-sm text-git-text-secondary">Report bugs and ask questions</div>
                </div>
                <ExternalLink className="h-4 w-4 text-git-text-secondary" />
              </a>
            </div>
          </div>

          {/* API Documentation Placeholder */}
          <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
            <h2 className="text-2xl font-semibold text-git-text-primary mb-4">REST API Documentation</h2>
            <div className="bg-git-bg-secondary border border-git-stroke-secondary rounded-lg p-6">
              <h3 className="text-lg font-semibold text-git-text-primary mb-3">Coming Soon</h3>
              <p className="text-git-text-secondary mb-4">
                Our REST API endpoints are currently under development. 
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default DeveloperDocsView;


