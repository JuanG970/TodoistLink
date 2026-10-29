# TodoistLink roadmap

Planning-only document. It proposes work; it changes no source, tests, dependencies, CI, or
infrastructure. Every item below needs its own scoped issue, implementation PR, and review.

Tracking issue: #1.

## Where the repository stands

- Three commits, all `2023-05-15` (`db347c9`, `d660eea`, `a28e245`); paclet version `0.0.1`
  (`TodoistLink/PacletInfo.wl:4`), `WolframVersion` `13.2+`.
- Kernel source: `TodoistLink/Source/TodoistLink.wl` (527 lines) and `ImportMarkdown.wl`
  (86 lines); 22 exported names.
- `TodoistLink/Tests/TodoistLink/Tests.wl` is the unmodified paclet template (16 lines,
  `SayHello`, zero `VerificationTest`).
- `README.md` is 13 bytes (`# TodoistLink`); `CHANGELOG.md` is empty; no `.github/` workflows;
  MIT `LICENSE`.
- 20 `TODO` markers in the kernel source; no `Documentation/` directory even though
  `PacletInfo.wl` declares a `Documentation` extension.

## How the evidence below was gathered

- Wolfram Engine 14.3 headless (`wolframscript -local -script`), paclet loaded with
  `PacletDirectoryLoad` on a checkout, `$UserBaseDirectory` redirected into a scratch tree. No
  Todoist credentials and no calls to `api.todoist.com`.
- Outgoing HTTP observed with a local echo server; the private endpoint symbols and
  `$TodoistAPIToken` were overridden for the probe only.
- API surface checked against the live docs: `https://developer.todoist.com/rest/v2/` redirects
  to `https://developer.todoist.com/api/v1/`, which documents `/api/v1/...` paths and
  `cursor` / `next_cursor` pagination.

## Candidates

### P1 — Make `NewTodoistTask` actually send a task

**Evidence.** `NewTodoistTask[content_String, opts]` builds its body from `"Description"` and
"Priority" only (`TodoistLink.wl:418-428`); the `content` argument is never placed in the
body and appears only in message parameters. The body is then encoded with
`ExportString[body, "RawJSON"]`, which on Engine 14.3 raises
`Export::jsonstrictencoding` for an `Association`; the request ends up with `Body -> $Failed`,
`General::erropts` is emitted, and the echo server received **no** request at all.

**Acceptance criteria.**

- A test captures the outgoing POST and asserts `content` is present, plus `description` and
  `priority` when supplied, and nothing else.
- Body encoding is association-safe (rule list, or a JSON export that accepts associations).
- The function returns a `TodoistTask` on success and a structured `Failure` otherwise, with no
  unhandled messages.

### P2 — Offline test harness that the rest of this list can lean on

**Evidence.** `Tests.wl` contains only the template `SayHello`; there is no test runner, no
`Makefile`, and no CI. Nothing in the repository currently asserts any behavior.

**Acceptance criteria.**

- One documented command runs the suite offline and exits non-zero on failure.
- At least 12 `VerificationTest`s covering: task-creation body (P1), project lookup (P3),
  missing-credential path (P5), URL construction, and close/reopen response handling.
- HTTP is stubbed through an injectable seam or overridable endpoint symbols; no network, no
  real token, no writes outside a temp directory.

### P3 — Project lookup and local cache correctness

**Evidence.** `getProjectByID` scans stored rows for the key `"project_id"` (`TodoistLink.wl:175`)
while rows are keyed by `"id"` (`columns`, `TodoistLink.wl:136`). Measured: after storing a
project with id 42, `getProjectByID[42]` returned `<|"name" -> "No project found"|>`;
`parseTodoistTask` attaches that value to every task (`TodoistLink.wl:292`). Separately,
`AddProjectToDB` does a plain `INSERT` (`TodoistLink.wl:141-148`) and callers wrap it in
`Quiet` (`TodoistLink.wl:162`), so a refetch collides on the primary key and is silently
dropped, leaving stale names.

**Acceptance criteria.**

- Lookup uses one key convention, with a regression test against a temporary HSQL database.
- Refresh is an upsert (or delete-then-insert) and surfaces failures instead of `Quiet`.
- Cache lifetime is documented: explicit refresh only, or refresh per listing call.

### P4 — Move from REST v2 to the v1 API

**Evidence.** Endpoints are hardcoded to `https://api.todoist.com/rest/v2` (`TodoistLink.wl:46-47`)
plus one absolute projects URL (`TodoistLink.wl:155`); the v2 documentation URL now redirects to
`/api/v1/`. No list call follows pagination — the seven `URLExecute` sites pass no `cursor` and do
not read `next_cursor`, so lists are silently truncated at the server's page size.

**Acceptance criteria.**

- One base-URL/version constant; no `rest/v2` literal remains.
- List functions follow `next_cursor` to exhaustion with a page cap, tested against two canned
  pages.
- Close/reopen paths and response shapes are verified against v1 and covered by tests.
- `CHANGELOG.md` entry and a version bump when the migration lands.

### P5 — Side-effect-free, idempotent initialization

**Evidence.** `InitTodoistLink[];` runs at file scope (`TodoistLink.wl:127`), so every
`Needs["TodoistLink`"]` creates `$TodoistFolder` (`FileNameDrop[$UserBaseDirectory, -1]`, which
resolves to `$HOME/TodoistLink` on the measured machine) and opens a live HSQL JDBC connection.
The schema is created only when that folder is absent: with a pre-existing folder,
`information_schema.tables` was empty and inserts failed with
`user lacks privilege or object not found: PROJECTS`. The
credential lookup is also unsound: `SystemCredential["TodoistAPIToken"]` returns `$Failed` when
unset, `MissingQ[$Failed]` is `False` (`TodoistLink.wl:21-29`), so the `AuthenticationDialog`
branch never runs and requests carry `Bearer $Failed`.

**Acceptance criteria.**

- Loading the paclet defines symbols only; an explicit `TodoistLinkInit[]` performs folder, DB,
  and schema work, and is idempotent across repeated calls and stale folders.
- A schema version marker drives create/migrate; failure produces one clear message.
- Absent or invalid credentials are detected with `MissingQ`/`FailureQ` and reported without a
  dialog, so headless use fails fast; tested against an empty credential store.

### P6 — Packaging and documentation baseline

**Evidence.** `README.md` is one heading, `CHANGELOG.md` is empty, the version is `0.0.1`, and
`PacletInfo.wl` advertises a `Documentation` extension while no `Documentation/` directory
exists. The only usage example is `TodoistLink_notebook.nb`.

**Acceptance criteria.**

- README covers purpose, installation, credential setup, three usage examples, and the offline
  test command.
- Either add documentation pages (guides plus symbol references) or drop the `Documentation`
  extension so the paclet stops advertising docs it does not ship.
- `CHANGELOG.md` starts with the current version and records each release.

## Suggested order

P2 first (it makes P1 and P3 verifiable), then P1, P3, P5, P4, P6. P4 touches the same call
sites as P1 and P3, so landing it after them avoids two rewrites of the request layer.

## Observed but not proposed

- `getTodoistLabels` and `parseTodoistColor` are exported while the source comment calls them
  private — the export surface needs one decision, not a refactor.
- `TodoistProjectTasks["ProjectID" -> id]` sends a GET with a `"Body"` association; on 14.3
  Wolfram converts that to query parameters, so it works today. Do not "fix" it blindly.
- `TodoistTask` `MakeBoxes` still contains placeholder `"Expanded thing 1: Hidden"` summary
  items.
- `ResourceFunction["ToAssociations"]` is evaluated at call time, which makes parsing depend on
  a resource fetch; a local conversion would remove that dependency.
