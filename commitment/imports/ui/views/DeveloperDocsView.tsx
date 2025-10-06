import React from "react";

const DeveloperDocsView: React.FC = () => {
  return (
    <div className="max-w-5xl mx-auto px-6 py-10">
      <h1 className="text-3xl font-inconsolata-bold mb-4">Developer Docs</h1>
      <p className="text-gray-300 mb-6">
        This section will host documentation for using our REST APIs. 
      </p>

      <div className="space-y-4">
        <section>
          <h2 className="text-2xl font-semibold mb-2">Getting Started</h2>
          <ul className="list-disc list-inside text-gray-300">
            <li>Base URL</li>
            <li>Authentication</li>
            <li>Sample requests</li>
          </ul>
        </section>
      </div>
    </div>
  );
};

export default DeveloperDocsView;


