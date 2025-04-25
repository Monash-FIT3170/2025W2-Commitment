
Hi! Please read below to see a summary of what this API contains:

---------------------------------------------------------
commitment.ts:
a file containing the main functions of the API


function fetchDataFrom(url: string):
an error-free version of promiseDataFrom that returns a maybe instead of raising an error (to be used outside definitely)

function promiseDataFrom(url: string):
returns formulateRepoData once it has cloned the repo to the local machine and 
then deletes the data after it has finished cloning

function formulateRepoData(url: string, filepath: string)
a function which returns a RepositoryData object based on a filepath local to the machine. 
You can use this object to capture certain user metrics, such as commits or vice versa.

function createGitRepoStream(url$):
creates an observable object which will emit the repo data once the url$ has emitted a url

---------------------------------------------------------
types.ts:
contains all the types related to storing Git information. Up for discussion

---------------------------------------------------------
parsers.ts:
contains all parser functions and monads to be used to parse stdout from the git CLI

---------------------------------------------------------
command.ts
a simple function library to help execute commands to the console in the specified directory

---------------------------------------------------------
git_commands.ts
a file dedicated to formulating git-related commands to be used in parsing information from git

---------------------------------------------------------
helpers.ts
a small function library to help transform data in a pure way

