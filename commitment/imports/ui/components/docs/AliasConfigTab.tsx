import React from "react";
import { GitBranch } from "lucide-react";

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

const AliasConfigTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <section
        id="section-5-1"
        className="space-y-6 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <div className="space-y-3">
          <div className="flex items-center gap-3">
            <GitBranch className="h-6 w-6 text-git-accent-primary" />
            <h2 className="text-2xl font-semibold text-git-text-primary">5.0 Alias Configuration</h2>
          </div>
          <p className="text-git-text-secondary">
            Contributors sometimes commit from multiple Git accounts. Alias Configuration lets you merge those
            identities so analytics and scaling attribute work to the correct person every time.
          </p>
          <p className="text-git-text-secondary">
            Upload a JavaScript Object Notation (JSON) file describing each contributor and their known usernames
            or email addresses. The configuration is stored per user account, so it applies across all analysed
            repositories.
          </p>
        </div>

        <div className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">5.1 Config JSON Structure</h3>
          <p className="text-git-text-secondary">
            Provide a configuration name and an <code className="rounded bg-git-bg-secondary px-1 py-0.5">aliases</code>
            array. Each entry maps one student to their recognised Git usernames and email addresses.
          </p>
          <div className="rounded-lg border border-git-stroke-secondary bg-git-bg-primary p-4 font-mono text-sm text-git-text-secondary">
            <pre className="whitespace-pre-wrap">
{`{
    "officialName": "Ishrat Kaur",
    "gitUsernames": [
        "QodeWiz",
        "ikau0004",
        "ishwiz"
    ],
    "emails": [
      "ishratk1123@gmail.com",
      "ikau0004@student.monash.edu"
    ]
}`}
            </pre>
          </div>
          <p className="text-git-text-secondary">
            Any commits from the listed usernames or email addresses are treated as contributions from the same
            student. Only replace the values (names, usernames, emails) &mdash; the property names and overall
            structure must stay the same.
          </p>
          <p className="text-git-text-secondary">
            Multiple students are defined by adding additional objects to the <code className="rounded bg-git-bg-secondary px-1 py-0.5">aliases</code>
            array. Here is an example of a complete configuration:
          </p>
          <div className="overflow-hidden rounded-lg border border-git-stroke-secondary bg-git-bg-primary">
            <pre className="whitespace-pre-wrap px-4 py-3 font-mono text-sm text-git-text-secondary">
{`{
  "name": "Monash Sem 2 2025",
  "aliases": [
    {
      "officialName": "Amy Tjea",
      "gitUsernames": [
        "AmyTjea"
      ],
      "emails": [
        "amycaitjea@gmail.com"
      ]
    },
    {
      "officialName": "Muhammad Yoonus Nazeem",
      "gitUsernames": [
        "YoonusNazz"
      ],
      "emails": [
        "mnaz0012@student.monash.edu"
      ]
    },
    {
      "officialName": "Nicholas Bisset",
      "gitUsernames": [
        "Densetsu152637"
      ],
      "emails": [
        "nbis0006@student.monash.edu"
      ]
    }
  ]
}`}
            </pre>
          </div>
          <p className="text-git-text-secondary">
            Keep the top-level <code className="rounded bg-git-bg-secondary px-1 py-0.5">name</code> and <code className="rounded bg-git-bg-secondary px-1 py-0.5">aliases</code>
            keys unchanged. Create the JSON file in any text editor and save it with the <code className="rounded bg-git-bg-secondary px-1 py-0.5">.json</code>
            extension, or download the template provided in the app and update it with your data.
          </p>
          <div className="rounded-md border border-blue-400/40 bg-blue-500/5 px-4 py-3 text-sm text-git-text-secondary">
            <strong className="text-git-text-primary">Tip:</strong> Alias configuration is available only to
            signed-in users and applies to every repository you analyse while it is active.
          </div>
        </div>
      </section>

      <section
        id="section-5-2"
        className="space-y-5 rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6"
      >
        <h3 className="text-xl font-semibold text-git-text-primary">5.2 Alias Config Walkthrough</h3>
        <p className="text-git-text-secondary">
          Follow these steps to upload or update your alias configuration:
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture28.png",
          "Overview of the Alias Configuration entry point for logged-in users."
        )}
        <p className="text-git-text-secondary">
          Click the highlighted profile menu icon to open account options.
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture29.png",
          "Profile menu with Alias Configuration option circled."
        )}
        <p className="text-git-text-secondary">Choose <span className="font-semibold text-git-text-primary">Alias Configuration</span> from the menu.</p>
        {imageCard(
          "/docs_images/aliasconfig/Picture30.png",
          "Alias Configuration landing screen."
        )}
        <p className="text-git-text-secondary">
          Download the template to customise offline, or upload your own JSON file. Drag and drop is also supported.
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture31.png",
          "Alias configuration upload dialog showing template and upload buttons."
        )}
        <p className="text-git-text-secondary">
          After uploading, confirm the preview. You can apply the configuration globally or analyse a different
          repository without saving.
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture32.png",
          "Configuration preview with Apply Configuration and Analyse options."
        )}
        <p className="text-git-text-secondary">
          Selecting <span className="font-semibold text-git-text-primary">Apply Configuration</span> stores the
          mapping to your account. Click <span className="font-semibold text-git-text-primary">Save Configuration</span>
          to finalise and review the confirmation screen.
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture33.png",
          "Confirmation screen after saving the alias configuration."
        )}
        <p className="text-git-text-secondary">
          If validation fails, review the error message. The example below indicates a missing <code className="rounded bg-git-bg-secondary px-1 py-0.5">name</code>
          field caused by incorrect JSON structure.
        </p>
        {imageCard(
          "/docs_images/aliasconfig/Picture34.png",
          "Alias configuration error message highlighting missing name field."
        )}
        <p className="text-git-text-secondary">
          Maintain the required field names and structure to ensure contributors are matched correctly. Double-check
          each email address and Git username before uploading.
        </p>
      </section>
    </div>
  );
};

export default AliasConfigTab;
