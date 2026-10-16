# lume

A durable substrate for disposable agent harnesses. Read `pitch.md` for the why; it's the design contract.

## Layout

- `bin/lume` — CLI launcher (Babashka)
- `src/lume/store.clj` — state under `LUME_HOME` (default `~/.lume`), append-only EDN session log
- `src/lume/status.clj` — `lume status`: fold the log, render one bounded screen
- `src/lume/agents.clj` — adapters; the only code that knows which agent CLI is in play
- `src/lume/cli.clj` — commands
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
- Lume is built with lume. Start every session with `lume status`: it is the handoff, so there is no handoff file. Pinned notes are standing lessons, open items are the backlog (`decide (human):` items wait for Stephen).
- Drive through lume: `lume item add --gate "bb test"`, `lume spawn <id>`, review the diff, commit. Record what you learn with `lume note`, and `lume note --pin` anything the next session must not forget. When status lacks something you needed, add an item for it.
