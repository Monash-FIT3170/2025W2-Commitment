

export const last = <T>(arr: T[]): T | undefined => 
    arr.length > 0 ? arr[arr.length - 1] : undefined

export const zip = <T, U>(arr1: T[], arr2: U[]): [T, U][] => 
    [...Array(Math.min(arr1.length, arr2.length))]
    .map((i) => [arr1[i], arr2[i]])
