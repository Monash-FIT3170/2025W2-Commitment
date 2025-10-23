import React from "react";
import { FileText } from "lucide-react";

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

const RESTAPITab: React.FC = () => (
  <div className="space-y-6">
    <section
      id="section-7-1"
      className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
    >
      <div className="space-y-3">
        <div className="flex items-center gap-3">
          <FileText className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">7.0 REST API</h2>
        </div>
        <p className="text-git-text-secondary">
          Commitment exposes a REST API for programmatic access. Use the navigation links to explore the detailed
          specification and integration examples.
        </p>
      </div>
      <div className="space-y-4">
        <p className="text-git-text-secondary">
          To get started, open the navigation bar and select <span className="font-semibold text-git-text-primary">Developer Docs</span>.
        </p>
        {imageCard(
          "/docs_images/restapi/Picture42.png",
          "Home page highlighting the Developer Docs link in the navigation bar."
        )}
        <p className="text-sm text-git-text-secondary">Home page</p>
        <p className="text-git-text-secondary">
          Scroll through the Developer Docs to access the full API reference, authentication details, and usage guides.
        </p>
        {imageCard(
          "/docs_images/restapi/Picture43.png",
          "Developer Docs page containing the REST API documentation."
        )}
        <p className="text-sm text-git-text-secondary">Developer Docs page</p>
      </div>
    </section>
  </div>
);

export default RESTAPITab;
