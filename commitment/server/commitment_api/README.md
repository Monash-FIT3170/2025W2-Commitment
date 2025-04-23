
Hi! Please read below to see a summary of what this API contains:

main functions:

function fetchDataFrom(url):
a function which returns a RepositoryData object based on a cloned URL. 
You can use this object to capture certain user metrics, such as commits or vice versa.


function fetchDataFromPath(filepath):
a function which returns a RepositoryData object based on a filepath local to the machine. 
You can use this object to capture certain user metrics, such as commits or vice versa.

function createGitRepoStream(url$):
creates an observable object which will emit the repo data once the url$ has emitted a url string

to be worked on as more features are implemented
