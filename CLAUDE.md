# caribouLocPrep -- repository conventions

Guidance for contributors and coding agents working in this repo. This is a SpaDES module that
downloads and cleans caribou collar locations from several provinces and territories in Canada, and
builds the caribou study areas from them (`studyAreaCaribou`, and `studyArea_juris` with one area per
jurisdiction). Downstream modules (e.g. `JWTurn/caribou_SSUD`) use those study areas. The repo is
**public** on GitHub (`gc-rmcinnes/caribouLocPrep`).

## People / authorship

- **JWT** = Julie W. Turner -- module author and main maintainer.

## Hard rules (non-negotiable)

- **Raw caribou locations are restricted.** This module handles them directly. Raw collar locations
  (and any table or object that still holds them, e.g. `caribouLoc`), and herd-range details not
  already public, never go into tracked files, commit messages, PR/issue text, test fixtures, or
  rendered docs. Tests use small made-up locations. Derived study-area polygons are uploaded to
  Drive, not committed. Keep data in `data/` (gitignored). When unsure, ASK JWT.
- **No host names, IP addresses or restricted terms in published content.** Enforced by
  `scripts/git-hooks/check-restricted-names` (lists in the gitignored `_restricted_terms.txt` and
  `.claude/r-project-policy.json`).
- **Secrets stay outside the repo.** Login tokens and keys (MoveBank credentials, Google Drive,
  GitHub) live outside the repo (e.g. `MOVEBANK_USERNAME` / `MOVEBANK_PASSWORD` in `~/.Renviron`,
  `~/.config/`); never read, print or copy them, and never pass them as values in committed code.
- **Never fabricate citations, references, or DOIs.** Verify against authoritative sources; ASK JWT
  if unsure.
- **Respect the existing code style.** Do not reformat code you were not asked to change.

## Checks before pushing

- Once per clone, install the pre-push check:
  `install -m 755 scripts/git-hooks/pre-push "$(git rev-parse --git-common-dir)/hooks/pre-push"`.
- Run it by hand on the whole repo: `scripts/git-hooks/check-restricted-names --tree`.
