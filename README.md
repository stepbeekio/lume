# lume

A durable substrate for disposable agent harnesses. Lume keeps a long agent task's state in an event log instead of in an agent's context: work items, gates, notes, and questions for you. Agents drive it through a CLI, and a fresh agent picks up from `lume status`. See [pitch.md](pitch.md) for the why.

Nothing is written inside your repo. All state lives under `~/.lume`.

## Install

Needs [Babashka](https://babashka.org), [bbin](https://github.com/babashka/bbin) and the `claude` CLI.

```
bbin install io.github.stepbeekio/lume
```

Re-run it to upgrade. To work on lume itself, run `bbin install .` from a checkout instead.

Then set up the agent side of the machine:

```
lume install
```

It writes a Claude skill, `~/.claude/skills/lume/SKILL.md` (or under `$CLAUDE_CONFIG_DIR`), that teaches any Claude session to work in a repo through lume: start from `lume status`, add gated items, spawn and drive, review and commit, ask you rather than allow, answer or push. It prints each file it wrote, updated or left unchanged, so it's safe to re-run after an upgrade.

It also prints one line for you to add yourself, because it never edits your settings: `"LUME_ACTOR": "claude"` in the `env` of `~/.claude/settings.json`. That makes interactive Claude sessions act as `claude` rather than as you, so lume refuses them `lume allow` and `lume answer`.

## Use

In any repo:

```
lume init "cut cold build time; tests stay green" --gate "make test" --allow "make"
lume item add "cache the dependency step"             # gets the default gate, make test
lume drive                                            # agents work until they need you
```

`--gate` is the session's default gate: items added without their own `--gate` get it, so none end up ungated by omission. Each `--allow` is a `lume allow`: a command workers may run.

For work with a number to move, give the item a measurement and take a baseline:

```
lume item measure i1 'make clean; /usr/bin/time -f "build_s=%e" make 2>&1 | tail -1'
lume measure i1                                       # m1 i1 build_s=41.2 · 41.3s
```

The command's `name=number` (or `name: number`) pairs become the row; with none, its last number is `value`. Wall time is always recorded. Every spawn on the item measures again before its gate runs, and status shows the latest row; `lume item show` has them all. A row shows each number's change against the latest earlier passing row of the same command, e.g. `m2 i1 build_s=38.9(-2.3) · 39.0s · vs m1`, so the baseline is the row before; changing the command starts a new baseline.

`lume drive` runs one fresh driver agent per turn. Each driver makes one decision: add an item, spawn a worker, review and commit a worker's diff, or ask you something. A worker's item closes only when its gate exits 0. The drive stops when it has a question for you, when no items are open, or when its turn budget runs out (10 by default).

Follow along in another terminal:

```
watch -n 2 --color lume status --color
```

Status is coloured at a terminal (set `NO_COLOR` to turn that off). `watch` isn't a terminal to lume, so pass `--color` to both.

When it stops on a question:

```
lume answer a1 "yes, drop the first run"
lume drive
```

Pushing is up to you. Drivers commit, but they can't push.

## Commands

```
lume install                   set up agents on this machine (the lume skill); safe to re-run
lume init "<goal>" [--gate "<cmd>"] [--allow "<cmd>"]...
lume status [--color]          one screen: goal, asks, pinned notes, items, recent events
lume item add "<intent>" --gate "<cmd>"
lume item close <id> done|dropped "<why>"
lume item show <id>            one item in full: intent, gate, disposition, its events
lume item measure <id> "<cmd>" measure the item after every spawn on it: one ledger row per run
lume measure <id>              run the measurement now (a baseline) and print its row
lume spawn <id>                run one worker on an item
lume drive [--turns N]         driver turns until an ask, no open items, or N turns
lume note [--pin] "<text>"     steering; pinned notes stay in status and every prompt
lume ask "<question>"          agents ask; only you answer
lume answer <id> "<text>"
lume allow [--revoke] "<cmd>"  only you grant permissions
lume log [-n N]                raw events
```

## Develop

Lume is built with lume: run `lume status` in this repo. Run the tests with `bb test`.
