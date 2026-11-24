# TodoistLink roadmap

Planning-only document. It proposes work; it changes no source, tests, dependencies, CI, or
infrastructure. Every item needs its own scoped issue, implementation PR, and review.

Tracking issue: #1.

## Where the repository stands

Verified 2026-11-24 on `main` @ `db347c9` (three commits, all dated 2023-05-15).

- Paclet version `0.0.1`, `WolframVersion` `13.2+` (`TodoistLink/PacletInfo.wl:4-5`).
- Kernel: `TodoistLink/Source/TodoistLink.wl` (527 lines) and `ImportMarkdown.wl` (86 lines);
  seven `URLExecute` call sites; 20 `TODO` markers.
- `TodoistLink/Tests/TodoistLink/Tests.wl` is still the paclet template: 16 lines, `SayHello`,
  zero `VerificationTest`.
- `README.md` is 13 bytes, `CHANGELOG.md` is 1 byte. No `Makefile`, no `.github/`, no CI.
- `PacletInfo.wl:8` declares a `Documentation` extension but no `Documentation/` directory exists.
- No `develop` branch; `main` is the only long-lived branch.

## How the evidence below was gathered

- Wolfram Engine 14.3.0 headless (`wolframscript -local -script`), paclet loaded with
  `PacletDirectoryLoad` against a worktree, `$UserBaseDirectory` redirected into `/tmp`, no Todoist
  credentials, no calls to `api.todoist.com`.
- Outgoing HTTP observed with a local echo server (`127.0.0.1:8899`); the private endpoint symbols
  and `$TodoistAPIToken` were overridden for the probe only.
- API surface checked against the publisher's own redirect on 2026-11-24:
  `https://developer.todoist.com/rest/v2/` returns `301` to `/api/v1/` (`200`).

## Candidates

### P1 — Make `NewTodoistTask` actually send a task

**Evidence.** `NewTodoistTask[content_String, opts]` builds its body from `"Description"` and
`"Priority"` only (`TodoistLink.wl:418-429`); the `content` argument is never placed in the body and
appears only in message parameters. The body is then encoded with
`ExportString[body, "RawJSON"]` (`TodoistLink.wl:440`). Root cause, measured: `Association` is
`HoldAllComplete`, so the `Association @ Flatten @ {If[OptionValue[...] ...]}` construction stays
inert and `ExportString` rejects it with
`Export::jsonstrictencoding: Expression Association cannot be exported as JSON`; the request is
built with `Body -> $Failed`, `General::erropts` follows, and the echo server received **no**
request at all. `ExportString[<|...|>, "RawJSON"]` works fine on 14.3 when the association is
already built, so the defect is in how the body is assembled, not in JSON export.

**Acceptance criteria.**

- A test captures the outgoing POST and asserts `content` is present, plus `description` and
  `priority` when supplied, and nothing else.
- The body is assembled as a list of rules (or otherwise evaluated before export); no
  `Export::jsonstrictencoding` under any option combination, including no options at all.
- The function returns a `TodoistTask` on success and a structured `Failure` otherwise, with no
  unhandled messages.

### P2 — An offline test harness the rest of this list can lean on

**Evidence.** `Tests.wl` contains only the template `SayHello`; there is no runner, `Makefile`, or
CI. Nothing in the repository asserts any behavior today.

**Acceptance criteria.**

- One documented command runs the suite offline and exits non-zero on failure.
- At least 12 `VerificationTest`s covering: task body construction (P1), project lookup and
  refresh (P3), the missing-credential path (P4), URL construction, and close/reopen handling.
- HTTP is reachable through an injectable seam (overridable endpoint symbols are enough); no
  network, no real token, no writes outside a temporary directory.

### P3 — Project lookup and local cache correctness

**Evidence.** `getProjectByID` scans cached rows for the key `"project_id"` (`TodoistLink.wl:171`)
while rows are keyed by `"id"` (`columns`, `TodoistLink.wl:136`). Measured: after storing a project
with id 42, `TodoistProjects` returned one row and `getProjectByID[42]` returned
`<|"name" -> "No project found"|>`; `parseTodoistTask` attaches that value to every task
(`TodoistLink.wl:280`). Separately, `AddProjectToDB` issues a plain `INSERT`
(`TodoistLink.wl:141-149`) and callers wrap it in `Quiet` (`TodoistLink.wl:162`), so a refetch
collides on the primary key and is silently dropped, leaving stale names. A realistic top-level
project (`parent_id` null in the API) also fails outright here: passing `Missing["NotAvailable"]`
raised `DatabaseLink\`JDBC::error: Illegal value` and the insert returned `$Failed`.

**Acceptance criteria.**

- Lookup uses one key convention, with a regression test against a temporary HSQL database.
- Refresh is an upsert (or delete-then-insert) and surfaces failures instead of `Quiet`.
- Null parent values are stored as SQL null, not `Missing[...]`.
- Cache lifetime is documented: explicit refresh only, or refresh per listing call.

### P4 — Side-effect-free, idempotent initialization

**Evidence.** `InitTodoistLink[];` runs at file scope (`TodoistLink.wl:127`), so every
`Needs["TodoistLink`"]` creates the app folder under the parent of the user base directory and opens
a live HSQL JDBC connection — measured: after a bare load, `/root/TodoistLink/todoistDB.hsqldb.*`
appeared. The schema is created only when that folder is absent; with a pre-created empty folder,
inserts failed and listing projects failed because the `Projects` table never existed. The
credential check is also unsound: with no stored credential the token evaluates to a failure
expression, for which `MissingQ` is `False` (`TodoistLink.wl:21-29`), so the
`AuthenticationDialog` branch never runs and requests carry a `Bearer` header whose value is
`$Failed`.

**Acceptance criteria.**

- Loading the paclet defines symbols only. An explicit `TodoistLinkInit[]` performs folder, DB, and
  schema work, and is idempotent across repeated calls and stale folders.
- A schema version marker drives create/migrate; failure produces one clear message.
- Absent or invalid credentials are detected with `MissingQ`/`FailureQ` and reported without a
  dialog, so headless use fails fast; tested against an empty credential store.

### P5 — Move from REST v2 to the v1 API

**Evidence.** Endpoints are hardcoded to `https://api.todoist.com/rest/v2`
(`TodoistLink.wl:46-47`) plus one absolute projects URL (`TodoistLink.wl:155`); the v2 documentation
URL now redirects to `/api/v1/`. No list call follows pagination: the seven `URLExecute` sites pass
no `cursor` and never read `next_cursor`, so lists are silently truncated at the server page size.

**Acceptance criteria.**

- One base-URL/version constant; no `rest/v2` literal remains.
- List functions follow `next_cursor` to exhaustion with a page cap, tested against two canned
  pages.
- Response shapes for tasks, projects, and close/reopen are verified against v1 and covered by
  tests.
- `CHANGELOG.md` entry and a version bump when the migration lands.

### P6 — Packaging and documentation baseline

**Evidence.** `README.md` is one heading, `CHANGELOG.md` is empty, the version is `0.0.1`, and
`PacletInfo.wl` advertises a `Documentation` extension with no pages behind it. The only usage
example is `TodoistLink_notebook.nb`.

**Acceptance criteria.**

- README covers purpose, installation, credential setup, three usage examples, and the offline test
  command.
- Either add documentation pages (guides plus symbol references) or drop the `Documentation`
  extension so the paclet stops advertising docs it does not ship.
- `CHANGELOG.md` starts at the current version and records each release.

## Suggested order

P1 first: it is the smallest change that makes the paclet do its advertised job, and it forces the
endpoint seam that P2 then formalizes. Then P2, P3, P4, P5, P6. P5 touches the same request-layer
call sites as P1 and P3, so landing it after them avoids two rewrites of the same code.

## Out of scope

Chatbook or notebook frontends, hosted Wolfram CI, non-HTTP transports, transcript storage, and
authentication/OAuth. This document also makes no claims about the Sync API migration mentioned in
`TodoistLink.wl:133`; that is a separate decision.

## Observed but not proposed

- `getTodoistLabels` and `parseTodoistColor` are exported while the source comment calls them
  private — the export surface needs one decision, not a refactor.
- `TodoistProjectTasks["ProjectID" -> id]` sends a GET with a `"Body"` association; on 14.3 Wolfram
  converts that to query parameters, so it works today. Do not "fix" it blindly.
- `createTodoistDB[]` declares `is_favourite` while `columns` uses `is_favorite`; values still line
  up positionally, so this is a naming trap rather than a live bug.
- `TodoistTask` `MakeBoxes` still contains placeholder `"Expanded thing 1: Hidden"` summary items.
- `ResourceFunction["ToAssociations"]` is evaluated at call time, so parsing depends on a resource
  fetch; a local conversion would remove that dependency.
