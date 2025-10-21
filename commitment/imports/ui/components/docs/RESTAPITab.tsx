import React from "react";
import { FileText } from "lucide-react";

const RESTAPITab: React.FC = () => (
  <div className="space-y-6">
    <section className="rounded-lg border border-git-stroke-primary bg-git-bg-elevated p-6">
      <div className="mb-4 flex items-center gap-2">
        <FileText className="h-6 w-6 text-git-accent-primary" />
        <h2 className="text-2xl font-semibold text-git-text-primary">6.0 REST API Documentation</h2>
      </div>
      <div className="space-y-6 text-git-text-secondary">
        <div className="space-y-4">
          <p>Hello! The purpose of this document is to give you some context about how this API works.</p>
          <p>It works like this:</p>
          <ul className="list-disc space-y-2 pl-6">
            <li>
              HTTP/WebSocket requests are handled by a request catcher, and are managed automatically by the operating system&apos;s thread scheduler.
            </li>
            <li>
              Your repository will be cloned to a local folder (only the git metadata) and a plethora of git log/show commands will retrieve metadata, parsing the command line outputs.
            </li>
            <li>The parsed information is logically coalesced into types and is sent back to you.</li>
          </ul>
        </div>

        <div id="rest-api-calling" className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">Calling the API</h3>
          <p>
            You can call the API via HTTP or using WebSockets to receive a reactive message. This gives you flexibility as
            to whether:
          </p>
          <ul className="list-disc space-y-2 pl-6">
            <li><strong>HTTP</strong>: you don&apos;t want to manage loading progress and just want the data.</li>
            <li><strong>WebSockets</strong>: you want to display loading progress that might give the user something to mull over while the data loads in the background.</li>
          </ul>
          <p>
            Both methods require you to send a payload of some sort. This payload should be in standard format, where git
            can clone the URL safely from the command line taking it as an argument:
          </p>
          <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-sm text-git-text-secondary">
https://github.com/&lt;owner&gt;/&lt;repo&gt;
          </pre>
          <p>
            If your repo is public, you have nothing to worry about—it should work automatically. However, if your repo is
            private, the request may time out and give you an error because git did not have permission to clone the repo.
            To fix this, ensure that you are providing an access key inside your payload, which will help git authentication
            approve cloning the repo. The general format for such a payload is the following (using https as an example):
          </p>
          <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-sm text-git-text-secondary">
https://&lt;username&gt;:&lt;access_key&gt;@github.com/&lt;owner&gt;/&lt;repo&gt;
          </pre>
        </div>

        <div id="rest-api-usage" className="space-y-6">
          <h3 className="text-xl font-semibold text-git-text-primary">How We Use It</h3>
          <div className="space-y-3">
            <h4 className="font-semibold text-git-text-primary">HTTP</h4>
            <p>We can interface with the API using code like this:</p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`fetch("http://" + API_CONN_ENDPOINT, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({ url }),
}).then((response) => {
  if (!response.ok) {
    reject("Haskell API returned status " + response.status);
  }

  response.json().then((d) => resolve(d.data));
});`}
            </pre>
            <p className="italic">Things to note: the method should always be POST, otherwise the API will reject the request.</p>
          </div>

          <div className="space-y-3">
            <h4 className="font-semibold text-git-text-primary">WebSockets</h4>
            <p>
              WebSockets can get a little complicated, so bear with us. For every new repository you want analytics on, you
              must create a new socket each time:
            </p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`const socket = new WebSocket("ws://" + API_CONN_ENDPOINT);`}
            </pre>
            <p>This ensures that reactive messages from other repositories do not get tangled, and requests are handled efficiently.</p>
            <p>You can set up the socket like this:</p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`socket.onopen = () => {
  if (notifier !== null) notifier.next("Connected to the API!");

  socket.send(
    JSON.stringify({
      url,
    })
  );
};`}
            </pre>
            <p>The socket can respond like this:</p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`socket.onmessage = (event) => {
  try {
    const { data } = event;
    const parsed = JSON.parse(data);

    if (parsed.type === "text_update" && notifier !== null) notifier.next(parsed.data);
    else if (parsed.type === "error") {
      reject(parsed.message);
      socket.close();
    } else if (parsed.type === "value") {
      resolve(parsed.data);
      socket.close();
    }
  } catch (err) {
    reject(err);
    socket.close();
  }
};`}
            </pre>
            <p>The API responses you receive can take three forms:</p>
            <ul className="list-disc space-y-2 pl-6">
              <li><code>text_update</code>: contains the most recent loading stage of your repository.</li>
              <li><code>error</code>: something went wrong inside the application.</li>
              <li><code>value</code>: the API has sent your analytics—resolve the promise and parse away.</li>
            </ul>
            <p>Guard for errors and close the socket after it is used:</p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`socket.onerror = () => {
  const s = "Encountered a Websocket Error";
  if (notifier !== null) notifier.next(s);
  reject(new Error(s));
  socket.close();
};`}
            </pre>
          </div>

          <div className="space-y-3">
            <h4 className="font-semibold text-git-text-primary">Types</h4>
            <p>
              Using TypeScript as an example, you can use these types directly. Keep in mind that <code>Map</code> objects are
              automatically converted to plain objects in JavaScript/TypeScript, so transpose them back to <code>Map</code>
              where convenient.
            </p>
            <pre className="overflow-x-auto rounded-md bg-git-bg-primary p-4 text-xs text-git-text-secondary">
{String.raw`type RepositoryData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: Map<string, CommitData>;
  contributors: Map<string, ContributorData>;
}>;

type BranchData = Readonly<{
  branchName: string;
  commitHashes: string[];
}>;

type CommitData = Readonly<{
  commitHash: string;
  commitTitle: string;
  contributorName: string;
  description: string;
  timestamp: string;
  fileData: FileChanges[];
}>;

type FileChanges = Readonly<{
  filepath: string;
  oldFilePath: string;
  char: ChangeType;
  likeness: number;
  newLines: number;
  deletedLines: number;
  diff: string[];
}>;

type ChangeType = "A" | "M" | "D" | "R" | "C";

type ContributorData = Readonly<{
  name: string;
  emails: string[];
}>;`}
            </pre>
          </div>
        </div>

        <div id="rest-api-further" className="space-y-4">
          <h3 className="text-xl font-semibold text-git-text-primary">Further Queries</h3>
          <p>
            For any further queries on how to call the API, leave comments on our GitHub page and we will do our best to
            respond in a timely fashion regarding your requests or inquiries.
          </p>
          <p>We thank you as a team for reading to the end of this document!</p>
        </div>
      </div>
    </section>
  </div>
);

export default RESTAPITab;
