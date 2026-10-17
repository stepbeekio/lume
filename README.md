# lume

A durable substrate for disposable agent harnesses. Lume keeps a long agent task's state in an event log instead of in an agent's context: work items, gates, notes, and questions for you. Agents drive it through a CLI, and a fresh agent picks up from `lume status`. See [pitch.md](pitch.md) for the why.

Nothing is written inside your repo. All state lives under `~/.lume`, and `lume clean` removes it: it lists what it would delete (the repo's state, any `refs/lume/*` and worktrees under that state), and deletes it only with `--yes`. A test runs a harness against a git+jj repo, cleans, and checks the repo is byte-identical to a copy taken before. `lume clean --session <id>` removes one session. Workers and drivers can't run it.

## Install

Needs [Babashka](https://babashka.org), [bbin](https://github.com/babashka/bbin) and an agent CLI: `claude` (the default) or `devin` (see [Devin](#devin) below).

```
bbin install io.github.stepbeekio/lume
```

Re-run it to upgrade. To work on lume itself, run `bbin install .` from a checkout instead.

Then set up the agent side of the machine:

```
lume install
```

It writes a Claude skill, `~/.claude/skills/lume/SKILL.md` (or under `$CLAUDE_CONFIG_DIR`), that teaches any Claude session to work in a repo through lume: start from `lume status`, add gated items, spawn and drive, review and commit, ask you rather than allow, answer or push. It prints each file it wrote, updated or left unchanged, so it's safe to re-run after an upgrade.

It also prints one line for you to add yourself, because it never edits your settings: `"LUME_ACTOR": "claude"` in the `env` of `~/.claude/settings.json`. That makes interactive Claude sessions act as `claude` rather than as you, so lume refuses them `lume allow` and `lume answer` before it even asks for a confirmation (see below).

### Devin

On a machine with [Devin CLI](https://cli.devin.ai) instead of Claude:

```
devin auth login                  # once, if devin auth status says you aren't
lume install --agent devin
```

That writes the same skill to `~/.config/devin/skills/lume/SKILL.md`, and lume's Devin config to `~/.lume/agents/devin.json`. It then prints two lines for your shell rc:

```
export LUME_AGENT=devin
alias devin='LUME_ACTOR=devin devin'
```

`LUME_AGENT` makes `devin` the default for `spawn`, `drive`, `run` and `install`, so you never pass `--agent`. A drive's workers run the drive's agent. The alias makes interactive Devin sessions act as `devin`, not you. Re-run `lume install` after an upgrade.

Lume drives Devin over ACP (`devin --config ~/.lume/agents/devin.json acp`). Devin's ACP sessions start in accept-edits mode, which would edit files and run commands it considers read-only without asking. Lume's config replaces your Devin user config for these runs, and its `ask` rules route every command and every write through lume. Lume answers each one from the allow list and logs it as a `:spawn/permission` event. Devin reports tokens, not dollars, so `lume budget --usd` doesn't limit its spawns; use `--spawns` and `--minutes`. Pass flags such as a model as `LUME_DEVIN_ARGS='["--model" "opus"]'`.

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

The command's `name=number` (or `name: number`) pairs become the row; with none, its last number is `value`. Wall time is always recorded. Every spawn on the item measures again before its gate runs, and status shows the latest row; `lume item show` has them all. A row shows each number's change against the latest earlier passing row of the same command, e.g. `m2 i1 build_s=38.9(-2.3) · 39.0s · vs m1`, so the baseline is the row before; changing the command starts a new baseline. A spawn on a measured item runs its gate but leaves the item open: keeping the change (`lume item close <id> done`) or rolling it back (`dropped`) is a decision for you or the driver, made from the row.

Noisy numbers want repeats: `lume item measure i1 'bb bench' --runs 10 --drop 1` runs the command ten times, drops the first (a warm-up), and records one row of each number's median with its min..max across the kept runs, e.g. `m3 i1 build_s=38.9(-2.3)[38.1..40.2] · 39.0s · 9 of 10 runs · vs m2`. A failing run ends the measurement (`fail exit 1 in run 4 of 10`), and all runs' output is in one file. Runs and drop are part of the measurement's spec, so changing them starts a new baseline, as changing the command does. Each row also records where it was taken (host, load average, git HEAD and jj change), which `lume item show` prints, so a noisy row can be explained.

Lower is better by default. For a number where higher is better (a score, a hit rate), name it: `lume item measure i1 'bb eval' --higher score` (repeat `--higher` for more). Rows mark it `score↑=0.82(+0.03)`, and the plateau count (below) treats a higher one as a new best. Changing the direction starts the plateau count over.

`lume drive` runs one fresh driver agent per turn. Each driver makes one decision: add an item, spawn a worker, review and commit a worker's diff, or ask you something. A worker's item closes only when its gate exits 0, and a measured one only when the driver closes it. The drive stops when it has a question for you, when no items are open, or when its turn budget runs out (10 by default).

Bound what agents spend with a budget:

```
lume budget --usd 20 --spawns 30 --minutes 120
```

Spend is the whole session's: dollars and minutes of every spawn and drive turn (cost from the agent's own result line), and how many spawns started. Status shows it on one line, e.g. `budget  $10.88/$20.00 · 22/30 spawns · 30.4/120 min`. Once a limit is reached, `lume spawn` refuses to start a worker and `lume drive` stops with the reason, as it does for an open ask. Anyone may add or lower a limit; raising one is yours alone, like `lume allow`. `lume budget` with no flags prints spend against the limits.

When control comes back to the driver is set with a cadence:

```
lume cadence batch 3 on gate-fail on plateau 4
```

`every-spawn` (the default) lets a driver turn request one spawn. `batch N` lets it request up to N, which run in order after the turn, saving driver turns. `on gate-fail` ends a batch at the first failed gate and hands back, logging the requests it skipped. `on plateau K` opens an ask once an item has K measurement rows in a row without a new best (lower is better, unless `--higher` names the number), so the drive stops for you. Status shows the cadence on the allow line. Your latest cadence is the ceiling: a driver may set its own to hand back as often or more often (e.g. `batch 2` under your `batch 3`), never less; going above it is yours alone, like raising a budget.

A session's harness (goal, default gate, allow, budget, cadence, and its items with their gates and measurements) is one page of EDN. `lume spec` prints the current session's, folded from the log, so a feedback loop that worked can be saved outside the repo and reused (`lume spec > ~/harnesses/build-perf.edn`; open items only, `--all` for closed too):

```clojure
{:goal "cut cold build time; tests must stay green"
 :gate "bb test"
 :allow ["bb test" "jj"]
 :budget {:usd 20 :spawns 30 :minutes 120}
 :cadence {:batch 3 :gate-fail true :plateau 4}
 :items [{:intent "cut cold build time" :measure "bb bench" :runs 10 :drop 1}
         {:intent "document the cache" :gate "bb lint"}
         {:intent "raise the cache hit rate" :measure "bb cache-stats" :higher ["hit_rate"]}]}
```

Only `:goal` is required; an item without `:gate` gets the spec's. `lume check <spec.edn>` validates one, whoever wrote it, and lists every problem with where it is (e.g. `[:items 0 :drop] must be less than :runs (3), got 3`), exiting 1 on any.

`lume run <spec.edn> ["<goal>"]` runs one in one command: it checks the spec and refuses on any problem, starts a session from it (the same events `init`, `item add`, `budget` and so on would log; a goal given here replaces the spec's), takes a baseline row for each measured item, then drives (`--turns N`, `--agent`). The harness is the same whoever drives it: `--turns 0` stops after the baselines, so you can drive by hand or `lume drive` later. A spec's `:allow`, and a `:cadence` above every-spawn, are yours as they are on the command line: you confirm them at a terminal, and an agent's `lume run` of such a spec is refused. A budget only adds limits to a fresh session, so anyone may set one.

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

`lume answer`, `lume allow`, `init --allow`, raising a budget and loosening the cadence are yours alone. Agents run as your user, so they could fake any environment variable or file lume reads. These commands therefore need an interactive terminal and a typed confirmation: retype the ask id to answer, or `yes` to change permissions. Spawns and drives have no terminal, so lume refuses them and says why.

Pushing is up to you. Drivers commit, but they can't push.

## Commands

```
lume install [--agent A]       set up agents on this machine (the lume skill); safe to re-run.
                               A: claude, claude-acp or devin; LUME_AGENT sets the default
lume init "<goal>" [--gate "<cmd>"] [--allow "<cmd>"]...
lume status [<session>] [--color]
                               one screen: goal, asks, pinned notes, items, recent events
                               (a past session's, by id from lume ls)
lume item add "<intent>" --gate "<cmd>"
lume item close <id> done|dropped "<why>"
lume item show <id>            one item in full: intent, gate, disposition, its events
lume item measure <id> "<cmd>" [--runs N] [--drop K] [--higher <name>]...
                               measure the item after every spawn on it: one ledger row per
                               measurement (N runs, first K dropped: medians and min..max);
                               lower is better unless --higher names the number
lume measure <id>              run the measurement now (a baseline) and print its row
lume spawn <id>                run one worker on an item
lume drive [--turns N]         driver turns until an ask, a budget limit, no open items, or N turns
lume budget [--usd N] [--spawns N] [--minutes N]
                               limit agent spend; agents may lower it, only you raise it
lume cadence [every-spawn | batch N] [on gate-fail] [on plateau K]
                               when drive hands back; agents may tighten it, only you loosen it
lume note [--pin] "<text>"     steering; pinned notes stay in status and every prompt
lume ask "<question>"          agents ask; only you answer
lume ask show <id>             one ask in full: question, who asked and when, its item, its answer
lume answer <id> "<text>"
lume allow [--revoke] "<cmd>"  only you grant permissions
lume check <spec.edn>          validate a harness spec: every problem, exit 1 on any
lume spec [--all]              this session's harness as spec EDN
lume run <spec.edn> ["<goal>"] [--turns N]
                               check a spec, start a session from it, baseline, drive
lume ls [--all]                this repo's sessions, newest first, one line each (10 unless --all)
lume log [-n N] [--session <id>]
                               raw events, of the current session or a past one
lume clean [--session <id>] [--yes]
                               remove every trace of lume for this repo (or one session)
```

## Develop

Lume is built with lume: run `lume status` in this repo. Run the tests with `bb test`.
