# apa7

R package for APA-style tables (flextable, Quarto, R Markdown).

## Conventions

Read `dev/CONVENTIONS.md` before adding or changing a function. It lists the
rules apa7 follows (argument names, return values, table structure,
formatting, notes, documentation, tests, NEWS) and a checklist for new
functions.

- Follow those rules. Refer to them by ID (e.g., ARG-7) in commit messages
  when relevant.
- When a decision sets or changes a convention, update `dev/CONVENTIONS.md`
  in the same commit.
- If a request conflicts with a rule, point out the rule and ask before
  breaking it.
- Departures found in existing code go in the "Open inconsistencies" table
  rather than being fixed silently.

## Other project files

- `dev/priorities.md`: roadmap, release checklist, and backlog.
- `vignettes/articles/_recentarticles.qmd`: the maintainer's scratch file.
  Do not edit it or report on it.
