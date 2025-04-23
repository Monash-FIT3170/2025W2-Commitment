

export const parse = <T>(text: string) => async function (parser: (text: string) => T) { return parser(text) }


