# Intro

Hello! The purpose of this document is to give you some context about how this API works.

It works like this:

HTTP/WebSocket requests are handled by a request catcher, and are managed automatically by
the operating system's thread scheduler.

Your repository will be cloned to a local folder (only the git metadata) and a plethora of
git log/show commands will retrieve metadata, parsing the command line outputs

The parsed information is logically coalated into types and is send back to you!

# How it works

## Calling the API from your application

You can call the API via HTTP or using Websockets to recieve a reactive message!

this gives you flexibility as to whether:
http: you don't want to manage any progress loading the repo and just want the data
ws: you want to display any loading progress that might give the user something to mull about
while the data loads in the background

Both methods require you to send a payload of some sorts. This payload should be in standard format, where
git can clone the url safely from the command line taking it as an argument:

```
https://github.com/<OWNER>/<REPOSITORY_NAME>
```

If your repo is public, you have nothing to worry about! It should work automatically.
However, if your repo is private, the request may time out and give you an error because git
did not have permission to clone the repo. To fix this, ensure that you are providing an access
key inside your payload, which will help git authentication to approve cloning the repo! The
general format for such a payload is the following (using https as an example):

```
https://<YOUR_USERNAME>:<YOUR_PRIVATE_KEY>@github.com/<OWNER>/<REPOSITORY_NAME>
```

# How our code interfaces with the API

# HTTP

We can interface with the api using code like this:

```
fetch("http://" + API_CONN_ENDPOINT, {
method: "POST",
headers: { "Content-Type": "application/json" },
body: JSON.stringify({ url }),
}).then((response) => {
if (!response.ok) reject(`Haskell API returned status ${response.status}`);
response.json().then((d) => resolve(d.data));
})
```

Things to note:
the method should always be POST, otherwise the API will reject the request

# WebSockets

Websockets can get a little complicated, so bear with me.
For every new repository you want analytics on, you must create a new Socket each time:

```
const socket = new WebSocket("ws://" + API_CONN_ENDPOINT)
```

this ensures that reactive messages from other repositories do not get tangled, and requests are handled efficiently
you can setup the socket like this:

```
socket.onopen = () => {

    // notify that connection to the api was successful
    if (notifier !== null) notifier.next("Connected to the API!");

    // send data through socket
    socket.send(
        JSON.stringify({
        url,
    })
    );

};
```

When the socket opens, you can safely send the payload containing the url you want to your analytics on

The socket can respond like this:

```
socket.onmessage = (event: WebSocket.MessageEvent) => {

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

};
```

You can see that the kind of data you are fetching can be three kinds:
parsed.type === "text_update": the data is a string which contains the most recent loading stages of your repository
parsed.type === "error": something went wrong inside the application
parsed.type === "value": the API has sent you your analytics! You can safely resolve the promise and parse away!

Make sure that you are also guarding for errors and are closing the socket after it is used up:

```
socket.onerror = (\_err: WebSocket.ErrorEvent) => {

    const s = "Encountered a Websocket Error";
    if (notifier !== null) notifier.next(s);
    reject(new Error(s));
    socket.close();

};
```

# Types

Using typescript as an example, you can use these types directly in your application:

PS: keep in mind that Map objects
are automatically converted to plain objects
in javascript/typescript
so you can use functions to transpose those types
to Map objects for convenience. They will work by
having key: value entries where the key will be a
parameter inside the object

This is the type that will be sent to you:
```
type RepositoryData = Readonly<{

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

}>;
```

# Final words

For any further queries on how to call from the API, leave comments on our github page and we will
do our best to respond in a timely fashion regarding your requests/inquiries

We thank you as a team for reading to the end of this document!
