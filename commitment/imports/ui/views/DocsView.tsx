import React, { useEffect } from "react";

const DocsView: React.FC = () => {
  // Clear repository history when user navigates to docs page
  useEffect(() => {
    localStorage.removeItem('lastRepoUrl');
  }, []);

  return (
    <>
      <h1 className="pt-12 pl-[12%] text-4xl font-bold text-git-text-primary">
        Documentation
      </h1>

      <div className="mx-auto mt-6 w-full max-w-[77%] bg-git-bg-elevated border border-git-stroke-primary rounded-lg shadow px-6 py-5">
        <p className="text-git-text-secondary">
          Documentation content will be added here.
        </p>
      </div>
    </>
  );
};

export default DocsView;
