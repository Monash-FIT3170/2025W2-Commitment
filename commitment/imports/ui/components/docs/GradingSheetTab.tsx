import React from "react";
import { FileSpreadsheet } from "lucide-react";

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

const GradingSheetTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <section
        id="section-6-1"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="space-y-3">
          <div className="flex items-center gap-3">
            <FileSpreadsheet className="h-6 w-6 text-git-accent-primary" />
            <h2 className="text-2xl font-semibold text-git-text-primary">6.0 Grading Sheet Upload</h2>
          </div>
          <p className="text-git-text-secondary">
            Grading sheets align repository metrics with assessment results. Upload a CSV or Excel file to calculate
            scaled grades directly within Commitment.
          </p>
        </div>

        <div className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">6.1 Sheet Format</h3>
          <p className="text-git-text-secondary">
            Ensure your grading sheet follows the required column structure before uploading.
          </p>
          {imageCard(
            "/docs_images/gradingsheet/Picture35.png",
            "Example grading sheet column layout."
          )}
        </div>
      </section>

      <section
        id="section-6-2"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">6.2 New Grading Sheet Upload</h3>
        <p className="text-git-text-secondary">
          After analysing a repository, open the Scaling view to upload a grading sheet.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture36.png",
          "Metrics page with navigation to the Scaling tab."
        )}
        <p className="text-sm text-git-text-secondary">Metrics page</p>
        <p className="text-git-text-secondary">
          If scaling has not yet been generated for this repository, you&apos;ll be prompted to configure it.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture37.png",
          "Generate Scaling modal prompting for configuration."
        )}
        <p className="text-sm text-git-text-secondary">Generate Scaling prompt</p>
        <p className="text-git-text-secondary">
          Once scaling options are selected, upload your grading sheet when prompted.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture38.png",
          "Upload grading sheet dialog with drag-and-drop area."
        )}
        <p className="text-sm text-git-text-secondary">Upload Grading Sheet prompt</p>
        <p className="text-git-text-secondary">
          After the file is processed, the final grade column reflects scaled grades for each student.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture39.png",
          "Scaled grades displayed after applying the grading sheet."
        )}
        <p className="text-sm text-git-text-secondary">Scaled grades</p>
      </section>

      <section
        id="section-6-3"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">6.3 Grading Sheet Download</h3>
        <p className="text-git-text-secondary">
          After scaling, download the updated grading sheet from the toolbar at the bottom of the Scaling view.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture40.png",
          "Toolbar showing the download grading sheet option."
        )}
      </section>

      <section
        id="section-6-4"
        className="space-y-4 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">6.4 Updating the Grading Sheet</h3>
        <p className="text-git-text-secondary">
          To refresh grades, either replace the existing sheet or create a new scaling configuration before uploading.
        </p>
        {imageCard(
          "/docs_images/gradingsheet/Picture41.png",
          "Options to replace the grading sheet or create new scaling."
        )}
      </section>
    </div>
  );
};

export default GradingSheetTab;
