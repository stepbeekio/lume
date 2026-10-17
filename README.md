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

The command's `name=number` (or `name: number`) pairs become the row; with none, its last number is `value`. Wall time is always recorded. Every spawn on the item measures again before its gate runs, and status shows the latest row; `lume item show` has them all. A row shows each number's change against the latest earlier passing row of the same command, e.g. `m2 i1 build_s=38.9(-2.3) · 39.0s · vs m1`, so the baseline is the row before; changing the command starts a new baseline. Once a spawn starts on the item, its before row (the latest such row taken before the spawn) is the baseline for every row after it, so re-measuring a change compares it with the code as it was, not with its own last row. `--before` (`:before true` in a spec) makes each spawn take that row itself, once it starts and before the worker edits anything, so every spawn yields a paired before/after: `m4 i1 build_s=41.0(-0.2) · 41.1s · vs m3 · before s2`, then `m5 i1 build_s=38.1(-2.9) · 38.2s · vs m4`. A before row doesn't count towards a plateau. A spawn on a measured item runs its gate but leaves the item open: keeping the change (`lume item close <id> done`) or rolling it back (`dropped`) is a decision for you or the driver, made from the row. `dropped` alone changes nothing in the repo; `dropped --revert "<why>"` restores the working copy to the jj commit of the item's before row (its git commit when there's no jj), and the close event records the row and commit. History is left alone: the rollback is a diff in the working copy, and jj keeps the dropped edits in its op log. The commit is read without snapshotting, so it is the working copy as of jj's last command; git restores only committed state.

Noisy numbers want repeats: `lume item measure i1 'bb bench' --runs 10 --drop 1` runs the command ten times, drops the first (a warm-up), and records one row of each number's median with its min..max across the kept runs, e.g. `m3 i1 build_s=38.9(-2.3)[38.1..40.2] · 39.0s · 9 of 10 runs · vs m2`. When a row and its base both have spreads, each change is marked: `(-2.3 ~)` when their min..max ranges overlap (within noise), `(-2.3 !)` when they don't (outside the spread); single-run rows get no mark. A failing run ends the measurement (`fail exit 1 in run 4 of 10`), and all runs' output is in one file. Runs and drop are part of the measurement's spec, so changing them starts a new baseline, as changing the command does. Each row also records where it was taken (host, load average, git HEAD and jj change), which `lume item show` prints, so a noisy row can be explained.

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
         {:intent "document the cache" :gate "bb lint" :items
          [{:intent "note the eviction rule"}]}
         {:intent "raise the cache hit rate" :measure "bb cache-stats" :higher ["hit_rate"]}]}
```

Only `:goal` is required; an item without `:gate` gets the spec's. An item's `:items` are its subitems (`--parent`), items like any other. `lume check <spec.edn>` validates one, whoever wrote it, and lists every problem with where it is (e.g. `[:items 0 :drop] must be less than :runs (3), got 3`), exiting 1 on any.

`lume run <spec.edn> ["<goal>"]` runs one in one command: it checks the spec and refuses on any problem, starts a session from it (the same events `init`, `item add`, `budget` and so on would log; a goal given here replaces the spec's), takes a baseline row for each measured item, then drives (`--turns N`, `--agent`). The harness is the same whoever drives it: `--turns 0` stops after the baselines, so you can drive by hand or `lume drive` later. A spec's `:allow`, and a `:cadence` above every-spawn, are yours as they are on the command line: you confirm them at a terminal, and an agent's `lume run` of such a spec is refused. A budget only adds limits to a fresh session, so anyone may set one.

Follow along in another terminal:

```
watch -n 2 --color lume status --color
```

Status is coloured at a terminal (set `NO_COLOR` to turn that off). `watch` isn't a terminal to lume, so pass `--color` to both.

Or don't watch at all: set `LUME_NTFY` to an [ntfy](https://ntfy.sh) topic URL (`export LUME_NTFY=https://ntfy.sh/<secret-topic>`) and subscribe to it on your phone. Each ask that opens (an agent's `lume ask`, a plateau's) and each drive that stops sends a short message with the ask id, question, item and session. Each send is logged as `:notify/sent`, or `:notify/failed` with why; a failed send never fails the command. The URL is never logged, since anyone who knows the topic can read it. Unset, nothing is sent.

Or answer from the phone: `lume drive --wait-on-ask` doesn't hand back on an open ask. It sends the ask with a one-time code and waits for a reply on the same topic, of the form `a12 <code> <answer>`; it skips the messages it posted itself (by the ids ntfy returns). Set `LUME_NTFY_REPLY` to read replies from a separate topic instead. The code is random, held only in the drive's memory and never logged, so a reply counts only if it names an open ask and quotes that ask's code: a leaked URL can't answer, and a stale reply can't land on a newer ask. A good reply is logged as `:ask/answered :by human :via :ntfy` and the drive carries on; `lume answer` at the terminal works too, and spends the code. A question ending `(y/n)` gets Yes and No buttons on the notification that post the reply for you, and one listing `(a) … (b) … (c) …` (as driver asks do) gets a button per option that posts its letter (two or three options: ntfy allows three buttons); answer anything else by publishing to the reply topic from the ntfy app or web app. A reply stream that goes silent for 100s (ntfy keeps it alive every ~45s) is dropped and reopened from the last message seen, so a half-dead connection can't hang the wait or lose a reply.

Set it up so the topic really is yours, since anything that can publish to it answers as you:

- Reserve the topic (and the reply topic, if you use one) on ntfy (or your own server) with deny-all access for everyone else, under a random name, and make an access token for lume. The phone app is logged in as you.
- Put the token in the URL, as ntfy's `?auth=` param or as user info: `export LUME_NTFY='https://:tk_…@ntfy.sh/<topic>'`. lume takes only URLs, so a self-hosted server works the same.
- Never use an open topic: anyone who can read it sees each ask's code, and anyone who can write it can then answer. The answer buttons carry the reply URL, token included, in the notification.
- Keep secrets out of questions: ntfy messages aren't end-to-end encrypted.

Neither URL reaches the processes lume starts (workers, drivers, gates): `LUME_NTFY*` is emptied in their env, so a worker can't read a code or post its own answer. lume sends its workers' asks itself.

When it stops on a question:

```
lume answer a1 "yes, drop the first run"
lume drive
```

`lume answer`, `lume allow`, `init --allow`, raising a budget and loosening the cadence are yours alone. Agents run as your user, so they could fake any environment variable or file lume reads. These commands therefore need an interactive terminal and a typed confirmation: retype the ask id to answer, or `yes` to change permissions. Spawns and drives have no terminal, so lume refuses them and says why.

Pushing is up to you. Drivers commit, but they can't push.

### Rules

`lume on <event-pattern> "<cmd>"` runs a shell command in the repo after each matching event. The pattern is an event type (`gate/ran`) or a namespace (`ask`, for every `ask/…` event). The event reaches the command in its env: the whole event as EDN in `LUME_EVENT`, and each plain value as `LUME_EVENT_<KEY>` (`:exit` is `LUME_EVENT_EXIT`, `:item/id` is `LUME_EVENT_ITEM_ID`). For example, to hear about every failed gate on your phone:

```
lume on gate/ran '[ "$LUME_EVENT_EXIT" = 0 ] || curl -s -d "gate failed on $LUME_EVENT_ITEM_ID: $LUME_EVENT_TAIL" "$LUME_NTFY"'
```

Rules are yours, like `lume allow`: adding or removing one needs a terminal and a typed `yes`. They run in your own lume process, with your env (`LUME_NTFY` included), never inside a worker or driver; a worker's events fire your rules in the `lume spawn` or `lume drive` that started it, when the worker exits. A rule's command acts as the rule (`LUME_ACTOR=r1`), so what it logs says which rule did it. Each run is logged as `:rule/ran` with its exit, time and last line of output (in full under the session's `rules/`). A rule that fails or hangs (killed after 60s, exit 124) never fails the command that triggered it. What a rule logs can fire rules in turn, up to a depth cap; a rule that would run past it logs `:rule/capped` instead. `lume on` lists the rules with each one's last run; `lume on --remove r1` drops one.

### Extensions

An extension is a feature of your own: events, folds, commands and status lines, in a `.clj` file in an `ext/` directory: `LUME_HOME/ext` for every repo (user), the repo's dir under `LUME_HOME` for every session of one repo (repo), or the session's (session); `lume ext` prints where. They load user first, then repo, then session, and a name can be taken only once. It is `<name>.clj` with ns `lume.ext.<name>`, and defines `feature`, a map like the built-in features':

```clojure
(ns lume.ext.tally
  (:require [lume.kernel :as kernel]))

(def feature
  {:events {:tally/added {:req #{:n}}}                     ; its event types, all :tally/…
   :init {:counts [] :gates 0}                             ; its state, (get-in state [:ext "tally"])
   :fold {:tally/added (fn [s e] (update s :counts conj (:n e)))
          :gate/ran (fn [s _] (update s :gates inc))}      ; it may fold any event, into its own state
   :guards {:tally/added (fn [s e] (when (neg? (:n e)) "refused: counts are positive"))}
   :reactions [{:on :tally/added :run (fn [e] (println "tallied" (:n e)))}] ; after an event, like rules
   :commands [{:path ["tally" "add"] :needs :session :usage "usage: lume tally add <n>"
               :run (fn [{:keys [session]} [n]]
                      (kernel/append! session (constantly {:type :tally/added :n (parse-long n)}))
                      (str "tallied " n))
               :help "lume tally add <n>    add a count"}]
   :status [{:lines (fn [{:keys [counts gates]}] [(str "total " (reduce + counts) " · gates " gates)])}]})
```

It loads into every lume command for the session only once you approve it: `lume ext approve tally` shows you the source, at a terminal, and loads it once you type its name. The approval (`:ext/approved`, yours alone like `lume allow`) names the file and its sha256, so an edited file stops loading until you approve it again. A session extension's approval is in the session's log; a user or repo one's is in the home log, `LUME_HOME/log.edn`, so it holds in every later session too; an unapproved, changed or refused one is skipped, and status says so in one line. `lume ext` lists every extension with its state.

An extension keeps to its corner, or it is refused (the reason shows in `lume ext`): its ns requires only `lume.kernel` and `clojure.string`/`set`/`edn`, and its code doesn't load, resolve or reach Java at run time; its name isn't a built-in's (`item`, `note`, …); its event types are `:<name>/…`; its folds see and return only `(get-in state [:ext name])`, and its guards see only that and guard only its own types; its commands are `lume <name> …`; each status section is `(fn [ext-state] lines)`, cut to 2 lines, and all extensions share 4. A reaction that throws prints a warning and never fails the command that triggered it. These checks are about shape, not a sandbox: an extension runs as you, so your reading of its source at approval is what trusts it. `test/lume/ext/tally.clj` is a complete one.

## Commands

```
lume install [--agent A]       set up agents on this machine (the lume skill); safe to re-run.
                               A: claude, claude-acp or devin; LUME_AGENT sets the default
lume init "<goal>" [--gate "<cmd>"] [--allow "<cmd>"]...
lume status [<session>] [--color]
                               one screen: goal, asks, pinned notes, items, recent events
                               (a past session's, by id from lume ls)
lume item add "<intent>" --gate "<cmd>" [--parent <id>]
                               --parent: work that emerged on an open item; status shows it
                               indented under it and lume spec nests it in the item's :items.
                               Nothing is inherited and nothing orders spawns: you choose
                               what runs next
lume item close <id> done|dropped "<why>"
lume item close <id> dropped --revert "<why>"
                               also restore the working copy to the jj commit (git without jj)
                               of the item's before row; logged on the close event
lume item show <id>            one item in full: intent, gate, disposition, its events
lume item measure <id> "<cmd>" [--runs N] [--drop K] [--higher <name>]... [--before]
                               measure the item after every spawn on it: one ledger row per
                               measurement (N runs, first K dropped: medians and min..max);
                               lower is better unless --higher names the number; --before
                               also measures when a spawn starts, before its edits
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
lume on <event-pattern> "<cmd>"
                               only you: run <cmd> after each matching event (gate/ran, ask, ...)
lume on [--remove <id>]        list rules with their last run, or drop one (only you)
lume ext                       extensions (user, repo, session): loaded, unapproved, changed or refused
lume ext approve <path|name>   only you, after reading its source: load it while it's unchanged
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
