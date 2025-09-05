# Contributing Guidelines

Thank you for your interest in contributing to this project!

- This document outlines the process and standards we follow to keep our work consistent and collaborative.
- Please read carefully before making contributions.
- Be respectful of others’ work.
- If you want to collaborate on a branch, raise a PR against that branch instead of pushing directly.
- For help, reach out to an SA or discuss in the team Discord.

## Commits

- Keep commits **small** and **meaningful**, one responsibility per commit.
- Use **brief and clear** messages describing the change.
- Commit frequently, but avoid noisy or vague messages.
- All commits must follow the **Conventional Commits** format: `type(scope): message`

| Type     | Description                                                               | Example                                        |
| -------- | ------------------------------------------------------------------------- | ---------------------------------------------- |
| Feature  | Introduces a new feature.                                                 | `feat(login): add remember me checkbox`        |
| Bug Fix  | Fixes a bug.                                                              | `fix(metrics): handle division by zero`        |
| Chore    | Routine tasks (e.g., build tools, package updates) not user-facing.       | `chore(deps): bump axios to 1.3.1`             |
| Docs     | Documentation changes only.                                               | `docs(readme): update setup instructions`      |
| Style    | Code style changes (e.g., formatting, indentation, semi-colons) no logic. | `style(ui): reformat button spacing`           |
| Refactor | Refactors code without changing functionality.                            | `refactor(auth): simplify token handling`      |
| Test     | Adds or updates tests.                                                    | `test(api): add unit tests for login`          |
| CI/CD    | CI/CD pipeline configuration changes.                                     | `ci(actions): add cache step for node_modules` |

More details on `type` of commits can be found [here](https://www.conventionalcommits.org/en/v1.0.0/#summary).

## Branching

### Locked Branches

- `main` is a locked branch and should never be directly commited to.
- All changes must go through the Pull Requests (PRs).

### Feature Branches

- Feature branches should be used to introduce changes.
- Use **dashes (`-`)** instead of spaces.
- Avoid **nested branches** (e.g., `feature/test/test-branch`).
- Members shouldn’t nest their branch more than what has been prescribed above.
- When feature branches are ready to be merged they must go through a PR.

### Branch Naming

- Branches should be follow the below naming convention: `type/change_name[/further_branch]`
- Use underscores (\_) for multi-word names
- Do not commit to another person’s branch without permission.
- Raise a PR other person’s branch with a clear message.

| Type     | Prefix      | Description                                                           | Example                                                   |
| -------- | ----------- | --------------------------------------------------------------------- | --------------------------------------------------------- |
| Feature  | `feat/`     | A new feature or enhancement.                                         | `feat/login_page` <br> `feat/dashboard_filter/add_button` |
| Bug Fix  | `fix/`      | A bug fix or patch.                                                   | `fix/metrics_bug` <br> `fix/graph_display/axis_label`     |
| Temp     | `temp/`     | Temporary work, experiments, or test branches.                        | `temp/form_validation`                                    |
| Chore    | `chore/`    | Routine tasks, dependency updates, minor tooling changes.             | `chore/update_eslint`                                     |
| Docs     | `docs/`     | Documentation-related updates only.                                   | `docs/api_docs`                                           |
| Style    | `style/`    | Code style changes (whitespace, formatting, naming), no logic change. | `style/header_formatting`                                 |
| Refactor | `refactor/` | Code restructuring that doesn't affect behavior.                      | `refactor/auth_flow`                                      |
| Test     | `test/`     | Adding or modifying tests.                                            | `test/user_service_tests`                                 |
| CI/CD    | `ci/`       | Continuous Integration/Deployment configuration changes.              | `ci/add_cache_step`                                       |

## Pull Requests (Merge Requests)

- **All changes** must go through a Pull Request.
- Share PRs via the `#pull-requests` Discord channel.
- PRs must be **squashed and merged** to keep history clean.
- A PR must be **reviewed and approved** before being merged into `main`.
- Provide a clear PR description (what changed, why, and testing notes if applicable).
- Template will be automatically applied to PRs so please use it.

### PR Checklist

- [ ] Code builds and runs locally inside the container.
- [ ] Test cases are written and passing.
- [ ] Lint and typechekcs pass.
- [ ] Commit messages follow Conventional Commits format.
- [ ] Branch follows naming conventions.
- [ ] PR description is clear and complete.
- [ ] Changes reviewed by at least one teammate.
