# Lume

*A durable substrate for disposable agent harnesses.*

**Status:** Draft 0.3 · **Purpose:** motivate the build

---

## The problem

Long-running agentic work needs coordination: deciding what to try next, carrying results forward, maintaining the feedback loop. Today that coordination happens *in context*, and that is what doesn't scale.

The best results I've had came from iterating with an agent the way a person iterates on their own work. Speeding up a build, I first had the agent write scripts that ran the build repeatedly and recorded each run in a CSV. From then on, every session ran one experiment against that loop and appended its outcome. The loop itself improved as we went. It worked very well, but I was the glue: starting each new session, restating the goal, pointing it at the ledger, deciding what came next. And I had rebuilt the same basic infrastructure (runners, ledgers, handoff notes) for the umpteenth time.

Handing coordination to a top-level coordinator agent works too, but it's a constant fight against context exhaustion. The coordinator does bookkeeping in its own context window: reading every subagent's output, remembering what's been tried, tracking which result came from which experiment. Compaction is the built-in answer, and it's a bad one: a lossy summary written by the agent that's already struggling.

**Lume succeeds if a multi-session task like the build-perf work runs with my involvement reduced to steering and review**: no hand-carried context between sessions, no feedback-loop infrastructure rebuilt from scratch, and a coordinator that doesn't drown.

**Who it's for.** Me first, on personal and work projects. Colleagues may see sessions later; they aren't the design target.

**Constraints.**
- *Laptop-scale.* CPU, disk and build caches are scarce; parallelism is a budget, not a default.
- *The agent CLI isn't always my choice.* At work it's Devin CLI. Nothing above a thin adapter may know which vendor is running.
- *Nothing touches the project.* It has to be usable in a work repo without anyone else adopting anything.

**Appetite.** Useful on a real task within days, not weeks. Lume must be something I use while it's being built; iterability beats completeness at every turn.

## The bet

Move coordination out of context and into data.

- **The substrate is durable and boring**: an event log, a ledger of measurements, a library of verifiers, adapters to agent CLIs, and the means to inspect all of it. It does the bookkeeping. Built carefully, versioned, invested in.
- **Harnesses are disposable data**: a page of EDN describing who runs, what gets measured, which checks gate progress, and when to ask for a decision. Written in minutes, by a human or an agent, deleted without regret.
- **Coordination is a role, not a component.** Whoever drives the lume CLI is the coordinator: me at a terminal, a Devin session, a Claude session. Lume doesn't know or care which. Whether I coordinate or hand it to an agent is a judgment about the task — how well it fits an obvious feedback loop — not an architectural decision.

Coordination isn't hard because the decisions are hard. It's hard because today it has no tools: no durable state outside the conversation, tools that flood context with raw output, infrastructure to rebuild before the real work starts, nothing that reports back except the agent itself. Agents are good at wielding tools. Give them the right one and coordination becomes ordinary tool use:

- **The tool holds the state.** `lume status` answers "what do we know so far?" in one screen: goal, ledger, recent verdicts, budget. An agent driver reads it instead of remembering.
- **The tool answers compactly.** Running an experiment appends a ledger row and prints that row, not a build log. Detail is there on request, never by default.
- **The driver acts by editing specs.** One spec edit — "ten runs, drop the first" — replaces dozens of in-context coordination turns. That's the force multiplier.
- **The driver is disposable.** All its state lives in the log, so an agent driver can be respawned for every decision: `devin -p "$(lume status)"` is a complete driver restart. Context exhaustion is a non-event.

What tools *don't* fix is genuine judgment: tasks with no clear metric, noticing the feedback loop itself is wrong, knowing when to stop or escalate. That's where I coordinate directly — through the same CLI.

This is the Erlang/OTP shape. Agents are unreliable processes; harnesses are supervision trees; the substrate is the runtime that makes crashing safe — including the supervisor's own crashes.

Agents writing harnesses is not the endgame; it's the core loop. In the build-perf episode, the agent's first act was to build its own feedback loop. Lume makes that loop a first-class, versioned artifact the driver can refine, constrained by a substrate that bounds what any harness can do.

Two principles underneath everything:

- **Everything is data.** Events, measurements, harness specs, verdicts — all EDN, all inspectable with the same tools, by humans and agents alike.
- **Agent-first output.** Every command's output is compact, structured, and bounded by default, expandable on request. A CLI an agent can drive without drowning is one a human enjoys too; the reverse isn't true.

## What using it feels like

You're in a work repo. Nothing about lume is installed there — it's a user-level tool, like git or ripgrep, living in your home directory with all its state.

```
$ lume run improve "cut cold build time; tests must stay green"
```

The harness says this task is agent-driven, so lume hands control to a Devin session with `lume status` as its prompt: the goal and an empty ledger. Its first act is to write a spec: a measurement (run the build five times, record wall time and cache hits) and a gate (the test suite). Lume runs a baseline and records it. The driver is respawned with the new status and proposes an experiment. A worker makes the change; lume measures, gates, and appends a row. Repeat.

Partway through, the driver notices variance is swamping the signal and edits the spec: ten runs, drop the first. That edit is an event like any other. You can see, afterwards, exactly when and why the feedback loop changed.

You check in when you like. `lume status` shows you the same screen the agent sees; you steer by editing the spec or leaving a note for the next decision. For a fuzzier task you'd skip the agent driver entirely and run the same commands yourself. What lands is an ordinary branch with ordinary commits, authored by you. `git status` is clean. Nobody had to adopt anything.

## Why this works

**The event log is the whole truth.** Everything that happens — spawn, tool call, measurement, verdict, decision, spec edit — is one line of EDN appended to a session log. Status, the ledger, replay, the UI and debugging are all projections of this one artifact. If you want to see something, there must be an event for it; if there's an event for it, every tool can see it.

**Status replaces compaction.** A fresh driver gets `lume status` — a projection of the log designed for the next decision — not a summary of a conversation it never had. Status is a deterministic function of the log, so it's testable and improvable like any other code, and it stays the same size whether the session is an hour or a week old.

**Verification and measurement are the product.** Harness quality is mostly feedback quality. Verifiers (artifact → pass/fail with evidence) and measurements (artifact → numbers) live in the substrate as a growing library: test suites, linters, diff-scope checks, timing runs, and (advisory only, never gating alone) LLM judges. Scripts an agent writes for one task can be promoted into the library for the next.

**Hand-back is configurable.** When lume returns control to the driver is part of the spec, not hard-coded: after every run, after a batch, on a gate failure, on plateau, on budget thresholds. If the driver is an agent, hand-back means respawning it with the current status; if it's me, it means a notification. Same event either way. A driver can set its own cadence ("try these three variants, then wake me") within limits I set. Batches save context; per-run decisions adapt faster; the spec says which.

**Harnesses are legible because they're small and declarative.** A harness names its agents, measurements, gates and cadence in one page of EDN. `lume check` validates it before anything runs — essential once agents are writing them. There is deliberately no planning DSL, no prompt framework, no cleverness; that lives in the agents.

**Agents are just processes, reached through one protocol.** Driving lume needs nothing but a shell — any agent that can run commands can coordinate, taught by a short skill or AGENTS.md snippet. Adapters matter only for the workers lume itself spawns. The primary adapter is an [Agent Client Protocol](https://agentclientprotocol.com) client: Devin CLI ships `devin acp` (JSON-RPC over stdio), and other agent CLIs speak it too. ACP gives the substrate structured tool-call events and permission requests without parsing vendor output; lume answers permission requests from policy in the spec, and logs them. A plain `-p`-style print adapter is the fallback. Messages are delivered at spawn: since agents are cheap, "send to X" means "spawn an X with this message as its task".

**Concurrency is a declared budget.** Sequential on one checkout is the default, which suits measurement work and laptops. A spec can declare a concurrency limit and opt into worktrees (created outside the repo) for genuinely independent work. Parallel builds that skew each other's timings are a measurement bug, not a speedup.

## The user experience, in layers

**The CLI is the daily driver.**

The same commands serve me and an agent driver; output is bounded by default, with flags to expand.

```
lume run <harness> [args]     run a harness against the current repo
lume status [<session>]       one screen: goal, ledger, verdicts, budget, notes
lume check <harness>          validate a spec
lume ls                       sessions for this project, with outcomes
lume repl [<session>]         a REPL inside the session
lume ui [<session>]           browser UI: live timeline, ledger, spec history, diffs
lume clean                    remove every trace: worktrees, refs, state
```

**The REPL is how you *intervene and invent*.** `lume repl` attaches an nREPL to a session — live or dead — with the log, the ledger and the substrate's own functions in scope. It's also how lume itself is built: the substrate is developed REPL-first, so changing lume while using it is the normal workflow, not a special one.

- A gate is behaving strangely mid-run? Attach, query the log, inspect the status the driver just saw.
- Not sure a harness idea works? Drive the substrate by hand — spawn one agent, run one measurement — and crystallize into a spec only what worked.
- The harness lacks a capability? Do that step manually; the gap becomes the next backlog item.

Harness specs are, in effect, saved REPL sessions — which is why they can stay a one-page format without a trapdoor into general programming: the trapdoor is the REPL.

**The web UI is how you *watch and share*.** A localhost server, rendering purely from the log: session list, live timeline, the ledger, spec history, per-agent diffs. The bar: a colleague who has never seen lume explains a session they didn't run, unaided, in under five minutes.

**Nothing touches the project.** All state — sessions, logs, ledgers, worktrees, agent exports — lives under your home directory, keyed by repo. Working refs hide under `refs/lume/`, invisible to `git branch` and refused for push. `lume clean` plus a diff against a control clone proves the footprint is zero, and that proof is a CI test.

## What lume refuses to be (v1)

No database as source of truth — the log is an EDN file; an index (Datalevin, Datascript) may be derived from it when queries hurt, and stays disposable. No daemon, server, or message bus — files and processes, until measured pain says otherwise (the log is the migration contract if that day comes; Kotlin is the pre-decided tool). No prompt library, planner, or RAG. No sandboxing beyond what the agent CLI provides. No npm — the UI is server-rendered HTML plus in-browser Clojure (scittle). No distribution; one machine.

## Building it on itself

**Building lume with lume is the test.** Not a paper exercise against imagined task shapes, and not a rebuild of one past workflow: every gap in `lume status` gets felt as friction while building the next feature. So nothing is designed further ahead than it's needed. Status starts deliberately naive and grows from use — whenever a driver (me or an agent) wanted something it didn't show, that's the next work item. This only works if status is cheap to change, which is why the substrate is REPL-first.

The general shape underneath is **a ledger of work items**: each has an intent, evidence (output, measurements, verdicts) and a disposition. A build-perf experiment, a module in a migration, a hypothesis in a bug hunt are all work items; harnesses add the task-specific parts.

Each stage must be usable on a real task the day it lands.

- **M0 — The kernel** *(hours).* The smallest thing worth writing by hand: a Babashka CLI (fast startup matters when an agent calls it constantly), an append-only EDN log per session, work items, notes, a naive `lume status`, and `lume spawn` running one worker. The first worker adapter is whatever's on the machine — Claude Code at home; Devin at work makes the second. Done when the next lume feature is built through `lume item add` → `lume spawn` → `lume status`, with nothing hand-carried in my head.
- **M1 — The loop** *(built with lume).* Gates and measurements on work items, specs, `lume check`, budgets, configurable hand-back, and a skill teaching an agent to drive the CLI. Done when an agent driver, respawned per decision, runs a multi-decision task — and improves its own feedback loop mid-task, visibly in the log.
- **M2 — Inspect & replay** *(built with lume).* `lume repl` attach to live and dead sessions, terminal replay, queries over the log, the ACP adapter and Devin CLI (proving vendor-neutrality). Done when a new harness is prototyped REPL-first before its spec exists.
- **M3 — The flip** *(ongoing).* Substrate development goes through harnesses by default; out-of-harness changes need written justification. Every gap that forces you outside is the backlog, self-generated.
- **M4 — Concurrency & UI** *(built by harnesses).* Concurrency budgets, opt-in worktrees, merging; then the web UI, whose views are parallel, independently verifiable work — the ideal proving load. If lume can't build its own UI pleasantly, that's the most important bug report the project can produce.

## Success criteria

1. I stop hand-carrying context: multi-session tasks run with my involvement limited to steering and review.
2. An agent driver runs a task of 20+ decisions without context exhaustion, because each decision is a fresh spawn reading `lume status`.
3. `lume status` stays under a fixed size (one screen) however long the session runs.
4. Switching a task between me driving and an agent driving needs no change to the harness.
5. Feedback loops outlive their tasks: at least one measurement an agent wrote for one task is reused by a later one.
6. Lume is used at work, through Devin CLI, on a real task.
7. A new workflow's harness fits on one page and runs with one command — whether a human or an agent wrote it.
8. Delete everything but the event log: status, the ledger, replay and the UI (minus diffs) still work.
9. The footprint test passes in CI: harness run, merge, clean — the target repo is indistinguishable from a control.
10. No component but adapters knows which agent CLI is in play.

## Risks, briefly

Driver thrash → budgets and hand-back limits I set, enforced by the substrate. Agent-written spec sprawl → schemas for every event and spec, enforced by `lume check`; new operations must subsume or retire an old one. Status that omits what matters, or bloats → status is code, versioned and replayable against old logs. Agent-first output drifting toward verbosity → size budgets on every command, tested. ACP immaturity → the print-mode fallback keeps every adapter at "spawn a process, read its output" in the worst case. Concurrent file writes → single-writer log, atomic renames. Noisy measurements on a laptop → sequential by default, and measurements record their environment. UI creep → no build step, and the UI stays a pure projection. And zero footprint is not zero disclosure — lume runs the agent CLIs you already run, as you; whether that's acceptable at work is a question about agent use, not about lume.

## Open questions (settle by building lume with lume, in the REPL where possible)

1. What goes in `lume status`, and how is it kept to one screen? Is one shape enough, or do workers and drivers need different projections?
2. How does the human steer mid-run: spec edits only, or a note channel that `lume status` surfaces?
3. Does ACP's multi-turn session model ever earn its keep over spawn-per-task?
4. Memory beyond the log: is a queryable projection of the log enough, or does cross-session memory (superseding claims with provenance) need its own store?
5. Merge strategy when concurrent workers overlap: interpreter-mediated, or a merger agent?
6. Per-agent budgets, or session-level only?
