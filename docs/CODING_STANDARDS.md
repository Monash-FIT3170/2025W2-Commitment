# Functional Programming (FP) Coding Standards

## Table of Contents
- [Functional Programming (FP) Coding Standards](#functional-programming-fp-coding-standards)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Core FP Dogmas](#core-fp-dogmas)
  - [Why We Do This](#why-we-do-this)
  - [Granular Functions in Practice](#granular-functions-in-practice)
  - [Currying and Composability](#currying-and-composability)
    - [Without Currying](#without-currying)
    - [With Currying](#with-currying)
  - [Data Purity and Immutability](#data-purity-and-immutability)
    - [Impure Code](#impure-code)
    - [Pure Code](#pure-code)
  - [Benefits of Functional Programming](#benefits-of-functional-programming)
  - [Naming Conventions and Code Style](#naming-conventions-and-code-style)
    - [General Rules](#general-rules)
    - [Variable Naming](#variable-naming)
    - [Function Naming](#function-naming)
    - [Comments and Documentation](#comments-and-documentation)
  - [Applying FP in This Codebase](#applying-fp-in-this-codebase)
  - [Future-Proofing Your Code](#future-proofing-your-code)
  - [Final Thoughts](#final-thoughts)


## Introduction

Hi!  
If you’re reading this, you’re probably interested in getting *gud* at FP (Functional Programming).  
This document outlines the mindset and principles behind FP, as well as how to write clean, readable, and maintainable code in this project.

## Core FP Dogmas

1. **Write as little code as possible** – build small, composable (granular) functions.  
2. **Maintain data purity** – avoid mutable state or side effects.  
3. **Focus on transformations, not state** – think in terms of *what changes* rather than *what stores data*.

## Why We Do This

- **Minimises runtime and state-related bugs** (e.g., server crashes).  
- **Speeds up Meteor compilation** by keeping code modular.  
- **Simplifies refactoring** – small, pure functions are easy to reason about.  
- **Improves error propagation** – well-designed functions naturally pass errors upward.

## Granular Functions in Practice

In FP, **granularity** means building functions that each perform *one specific task*.  
When you compose them, small, single-purpose functions create complex behaviour.

In the Haskell API, many functions are just one line long, but each builds upon others.  
That’s the essence of FP: power through simplicity and composition.

You can see similar structure in our TypeScript code under `commitment.server/helper_functions`.

## Currying and Composability

Currying allows partial application of functions — it’s one of FP’s secret weapons.

### Without Currying
```typescript
const r1 = execCommand(localPath, c1);
const r2 = execCommand(localPath, c2);
```

### With Currying
```typescript
const executeLocally = execCommand(localPath);
const r1 = executeLocally(c1);
const r2 = executeLocally(c2);
```

This makes your code:
- More **readable** and **concise**
- Easier to **reuse**
- Better aligned with FP principles

> 📖 Learn more: [Currying (Wikipedia)](https://en.wikipedia.org/wiki/Currying)

## Data Purity and Immutability

Variables should be **immutable**, once defined, they shouldn’t change.  
This ensures your data is “pure” and predictable.

### Impure Code
```typescript
let a: number | undefined = undefined;

switch (text) {
  case "yeet": a = 69; break;
  case "yo mama": a = 420; break;
}
// a could still be undefined 😬
```

### Pure Code
```typescript
const a: number = text === "yeet" ? 69 : 420;
```

Immutable data avoids bugs, unexpected state changes, and race conditions.

## Benefits of Functional Programming

- **Predictable behaviour** – no hidden state changes.  
- **High performance** – small, stateless functions are easily optimized.  
- **Reusable components** – composable building blocks simplify new features.  
- **Clean architecture** – modularity leads to better readability and testability.

## Naming Conventions and Code Style

Consistent naming and structure make the codebase easier to understand, debug, and extend.

### General Rules
- Use **camelCase** for variables and function names.  
- Use **PascalCase** for components, classes, and TypeScript types/interfaces.  
- Avoid abbreviations unless they’re *obvious* (e.g., `id`, `url`).  
- Always use **descriptive names** that express intent rather than type.

### Variable Naming
| Type             | Convention                           | Example                                 |
| ---------------- | ------------------------------------ | --------------------------------------- |
| Boolean          | Prefix with `is`, `has`, or `should` | `isLoading`, `hasAccess`, `shouldRetry` |
| Function         | Use a **verb** or action phrase      | `fetchUserData()`, `validateInput()`    |
| Constant         | Use `const` and meaningful names     | `const MAX_RETRY_COUNT = 3;`            |
| Pure Data        | Nouns describing data                | `userProfile`, `repositoryList`         |
| Temporary Values | Keep short but clear                 | `tempResult`, `parsedValue`             |

### Function Naming
- Keep functions **short and descriptive** – name them after their purpose, not their implementation.  
- Pure functions should describe **what** they do, not **how** they do it.  
  - Good - `filterValidRepos()`  
  - Bad - `loopThroughRepoListAndRemoveInvalidOnes()`
- Higher-order or curried functions can be prefixed by their action:  
  - `createFetcher()`, `buildParser()`, `makeHandler()`

### Comments and Documentation
- Use comments **to explain why**, not **what**.  
- If a function’s intent isn’t clear, refactor its name before adding comments.  
- For public or reused functions, include JSDoc-style annotations:
  ```typescript
  /**
   * Filters repositories by language.
   * @param repos - List of repositories.
   * @param language - Language to filter by.
   * @returns Repositories matching the language.
   */
  const filterByLanguage = (repos, language) => repos.filter(r => r.lang === language);
  ```

## Applying FP in This Codebase

- Use **small, composable** functions wherever possible.  
- Avoid **global mutable variables**, keep data flow predictable.  
- Employ **currying and pure transformations** to simplify logic.  
- Prefer **functional composition** over nested callbacks or loops.  
- Let **errors bubble up** rather than catching everything prematurely.  
- Structure code consistently using **imports**, **aliases**, and **naming conventions** defined above.

## Future-Proofing Your Code

When adding new features or refactoring:
- Keep functions *pure* and *predictable*.  
- Avoid deeply nested logic.  
- Extract logic into helpers when reused.  
- Ensure type safety through TypeScript annotations.  
- Document functions that affect shared data or critical processes.

## Final Thoughts

Functional Programming isn’t just a style, it’s a philosophy focused on clarity, composability, and correctness.  

By writing **pure**, **granular**, and **well-named** functions, you’ll:
- Write cleaner, safer code  
- Spend less time debugging  
- Help future contributors (and yourself) understand the code instantly

So go forth and be functional!
 
**Happy Coding!**
