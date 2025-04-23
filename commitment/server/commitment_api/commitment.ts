
import { Observable, Subject, defer, map } from "rxjs";

import { RepositoryData } from "./types";
import { executeCommand, doNotLogData, defaultStdFail, Command } from "./command"
import { parse } from "./parsers"


export async function fetchDataFrom(url: string): Promise<RepositoryData | null> {

	// see if repo exists

	// if it exists, clone to a path by duplicating from link

	// validate cloning

    // parse repo into struct
    return fetchDataFromPath("")
}

export async function fetchDataFromPath(path: string): Promise<RepositoryData | null> {
	// parse repo into struct
    return null
}


export function createGitRepoStream(url$: Observable<string>): Observable<RepositoryData | null> {
	const o$ = new Observable<RepositoryData | null>()

	url$.pipe(
		map((url: string) => {
			o$.pipe(
				map(async function() { await fetchDataFrom(url) })
			)
		})
	);
	
	return o$
}




