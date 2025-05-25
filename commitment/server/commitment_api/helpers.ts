

export const last = <T>(arr: T[]): T | undefined => 
    arr.length > 0 ? arr[arr.length - 1] : undefined

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] => 
    Array.from({ length: Math.min(...lists.map(list => list.length)) }, (_, i) =>
        lists.map(list => list[i]) as { [K in keyof T]: T[K][number] }
    )
