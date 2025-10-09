# Linting & Formatting Guide

This project uses **ESLint** to detect code issues and **Prettier** for formatting.

Before running any commands, make sure you’re in the correct folder:

```bash
cd commitments
```

The ESLint config and `src` folder both live here.

## 🔍 Check for Errors

- **Check the whole project:**
  ```bash
  npx eslint .
  ```

- **Check a specific file:**
  ```bash
  npx eslint path/to/yourFile.ts
  ```

- **Show only errors (ignore warnings):**
  ```bash
  npx eslint . --quiet
  ```

- **Auto-fix problems (where possible):**
  ```bash
  npx eslint . --fix
  ```

## ✨ Recommended VS Code Setup

Install these extensions:

- [ESLint](https://marketplace.visualstudio.com/items?itemName=dbaeumer.vscode-eslint)
- [Prettier](https://marketplace.visualstudio.com/items?itemName=esbenp.prettier-vscode)