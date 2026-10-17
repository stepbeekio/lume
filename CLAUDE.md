# lume

A durable substrate for disposable agent harnesses. Read `pitch.md` for the why; it's the design contract.

## Layout

- `bin/lume` — CLI launcher (Babashka)
- `src/lume/store.clj` — state under `LUME_HOME` (default `~/.lume`), append-only EDN session log
- `src/lume/kernel.clj` — what every feature shares: actor, fail!, flags, human-only!, append!/project/reactions, command dispatch (`run`), running commands in the repo
- `src/lume/status.clj` — the status frame: render the features' sections as one bounded screen
- features, each a namespace requiring `lume.kernel` (never `lume.cli`) with its events, folds, status and commands: `session` (init), `item`, `ask`, `note`, `measure`, `agent` (spawn, allow), `policy` (budget, cadence), `drive`, `ntfy`, `spec` (check, spec, run), `inspect` (status, ls, log, replay, repl), `install`, `clean`
- `src/lume/features.clj` — the built-in features, in order
- `src/lume/agents.clj` — adapters; the only code that knows which agent CLI is in play
- `src/lume/cli.clj` — `-main`: binds the built-in features, calls `kernel/run`
- `test/lume/` — `bb test`

## Feedback loop: REPL first

1. Find a running nREPL: `clj-nrepl-eval --discover-ports`. If there is none, start one in the background: `bb -cp src:test nrepl-server 1667`.
2. Evaluate with `clj-nrepl-eval -p <port> "<code>"`. After editing a file, reload it: `(require 'lume.status :reload)`. Try functions on real data (e.g. render a session log) before writing tests.
3. Before calling work done, run `bb test`.

Delimiter errors in `.clj` edits are repaired by hooks in `.claude/settings.json`.

## Rules the design depends on

- The log is the whole truth. New state means a new event type, never a side file or database.
- Output is agent-first: short, structured, bounded. `lume status` must stay one screen (there's a test).
- Nothing is written inside the target repo; all state lives under `LUME_HOME`.
- Vendor-specific code lives only in `lume.agents`.
- Build what's needed now; the pitch's later milestones are not a spec.

## Working here

- Version control is jj (colocated with git): `jj st`, `jj commit -m`.
- Lume is built with lume. Start every session with `lume status`: it is the handoff, so there is no handoff file. Pinned notes are standing lessons, open items are the backlog, and asks are questions waiting for Stephen (`lume ask "<question>"`; only Stephen runs `lume answer`).
- Claude sessions are not the human: interactive ones act as `claude` (`LUME_ACTOR=claude` in the env of `~/.claude/settings.json`, which Stephen sets; never edit `~/.claude`), spawns and drives as `s<n>`/`d<n>`. Lume refuses `lume allow` and `lume answer` to every actor but `human`, so leave asks and permissions to Stephen.
- Drive through lume: `lume item add --gate "bb test"`, `lume spawn <id>`, review the diff, commit. Record what you learn with `lume note`, and `lume note --pin` anything the next session must not forget. When status lacks something you needed, add an item for it.
