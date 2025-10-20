import { DependencyList, useEffect, useState } from "react";

interface UseAsyncState<T> {
  loading: boolean,
  promise?: Promise<T>
}

/**
 * Another one of those silly little hooks to help make it easier to hold derived data
 * @param asyncFn The async function to calculate derived data from
 * @param deps The list of dependencies to the given async function. The method will re-run whenever any dep changes.
 */
function useAsync<T, TErr=unknown>(asyncFn: () => Promise<T>, deps: DependencyList) {
  const [data, setData] = useState<T | null>(null);
  const [error, setError] = useState<TErr | null>(null);
  const [state, setState] = useState<UseAsyncState<T>>({
    loading: false,
  });

  // const makeFakePromise = () => {
  //   let resolveRaw: null | ((value: T | PromiseLike<T>) => void) = null;
  //   let rejectRaw: null | ((reason?: unknown) => void) = null;
  //
  //   const promise = new Promise<T>((resolve, reject) => {
  //
  //   });
  // }

  const setLoading = (loading: boolean) => setState(v => ({...v, loading}));

  useEffect(() => {
    let cancelled = false;
    const promise = asyncFn()
      .then(res => {

        return res;
      })

    promise
      .then(res => !cancelled && setData(res))
      .catch(err => {!cancelled && setError(err)})
      .finally(() => !cancelled && setLoading(false));

    setState({
      loading: true,
      promise: promise
    });

    return () => { cancelled = true; };
  }, deps);

  return { data, error, loading: state.loading, promise: state.promise };
}
