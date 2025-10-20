import { DependencyList, useCallback, useEffect, useState } from "react";

/**
 * Checks that a given value is actually an object, not an array, or a function, or something else.
 * @param value The value to check is an object
 * @returns True if value is an object, false otherwise.
 */
function isObject(value: unknown): boolean {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

/**
 * Like useState, but persists using localStorage
 *
 * @param key The local storage key to use
 * @param initialValue The initial value to assign when there is nothing in local storage
 * @param dependencies Dependencies to update the value from local storage when changed
 */
export function useLocalStorage<T>(key: string, initialValue: T, dependencies: DependencyList = [key]) : [T, (newValue: T | ((previousValue: T) => T)) => void] {
  const [value, setValue] = useState<T>(initialValue)

  useEffect(() => {
    const storedValueJSON = localStorage.getItem(key);
    if (storedValueJSON === null || storedValueJSON === undefined || storedValueJSON === "undefined") {
      setValue(initialValue);
      return;
    }

    // Parse JSON and do some checks to make sure it is the correct type
    const storedValue = JSON.parse(storedValueJSON)
    if (storedValue === undefined) {
      setValue(initialValue);
      return;
    }

    // Ensure basic types are the same
    if (typeof value !== typeof initialValue) {
      setValue(initialValue);
      return;
    }

    // Check to ensure arrays stay the same type
    else if (Array.isArray(value) !== Array.isArray(initialValue)) {
      setValue(initialValue);
      return;
    }

    // Enforce that objects need to have the same keys
    else if (isObject(value) && isObject(initialValue)) {
      const storedValueKeys = Object.keys(value as Object);
      const objectsMatch = Object.keys(initialValue as Object).every(v => storedValueKeys.includes(v));

      if (!objectsMatch) {
        setValue(initialValue);
        return;
      }
    }

    // This value was successfully retrieved from local storage.
    setValue(storedValue);
  }, dependencies)

  const lsSetValue = useCallback((newValue: T | ((previousValue: T) => T)) => {
    const evaluatedNewValue = newValue instanceof Function ? newValue(value) : newValue

    setValue(evaluatedNewValue);
    localStorage.setItem(key, JSON.stringify(evaluatedNewValue));
  }, [key, value]);

  return [value, lsSetValue];
}