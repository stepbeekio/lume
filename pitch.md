# Lume

*A durable substrate for disposable agent harnesses.*

**Status:** Draft 0.2 · **Purpose:** motivate the build

---

## The idea

Agentic coding works when it's harnessed: feedback loops, delegation to subagents, verification gates. But every harness today is bespoke — orchestration, messaging, and memory rebuilt each time, opaque to everyone including its author.

Lume splits the problem along its natural seam:

- **The substrate is durable and boring**: an event log, shared memory, mailboxes, a library of verifiers, and the means to inspect all of it. Built carefully, versioned, invested in.
- **Harnesses are disposable data**: a page of EDN describing who runs, how work fans out, and which checks gate progress. Written in minutes, deleted without regret.

This is the Erlang/OTP shape. Agents are unreliable processes; harnesses are supervision trees; the substrate is the runtime that makes crashing safe. And because harnesses are *data*, not code, the endgame is available cheaply: agents writing harnesses for agents, constrained by a substrate that bounds what any harness can do.

One principle underneath everything: **everything is data.** Events, messages, memory, harness specs, verdicts — all EDN, all inspectable with the same tools, by humans and agents alike.

## What using it feels like

You're in a work repo. Nothing about lume is installed there — it's a user-level tool, like git or ripgrep, living in your home directory with all its state.

```
$ lume run refactor "extract the parser from core"
```

A planner agent spawns in its own git worktree (created outside the repo), breaks the goal into steps. Workers fan out, one worktree each. Each worker's output must pass the gates you declared — the test suite, the linter — before it counts; failures respawn with the evidence attached. A reviewer sees the merged diff. What lands is an ordinary branch with ordinary commits, authored by you.

While it runs, `lume ui` shows the swarm live in your browser: a timeline of events, the spawn tree, verdicts and their evidence, each agent's diff. When it's done, the session is a file of events you can replay, query, or show a colleague.

Your teammates see a normal branch. `git status` is clean. Nobody had to adopt anything.

## Why this works

**The event log is the whole truth.** Everything that happens — spawn, message, artifact, verdict — is one line of EDN appended to a session log. Communication, memory, the UI, replay, and debugging are all projections of this one artifact. The discipline this enforces is the design: if you want to see something, there must be an event for it; if there's an event for it, every tool can see it.

**Verification is the product.** Harness quality is mostly verdict quality. Verifiers — functions from artifact to pass/fail-with-evidence — live in the substrate as a permanent, growing library: tests, linters, diff-scope checks, and (advisory only, never gating alone) LLM judges. New harnesses inherit the whole library. The durable investment goes here, not into prompts.

**Harnesses are legible because they're small and declarative.** A harness names its agents, its flow, and its gates in one page of EDN. A newcomer predicts its behavior by reading it. An agent can generate one, and `lume check` validates it before anything runs. There is deliberately no planning DSL, no prompt framework, no cleverness — that lives in the agents.

**Agents are just processes.** An agent is anything spawned with cwd set to its worktree: a Claude Code invocation, a shell script, a verifier. Agent CLIs are cwd-oriented, so isolation comes free. LLM-specific glue (structured output parsing, token accounting, permission flags) is quarantined in small per-vendor adapters; the substrate never knows which vendor — if any — is behind an agent. Messages are delivered at spawn: since agents are cheap, "send to X" simply means "spawn an X with this message as its task."

## The user experience, in layers

**The CLI is the daily driver.**

```
lume run <harness> [args]     run a harness against the current repo
lume check <harness>          validate a spec
lume ls                       sessions for this project, with outcomes
lume ui [<session>]           browser UI: live timeline, swarm graph, memory, diffs
lume repl [<session>]         a REPL inside the session
lume clean                    remove every trace: worktrees, refs, state
```

**The web UI is how you *watch and share*.** A localhost server, rendering purely from the log and memory: session list, live timeline, the spawn tree with clickable gates, a Datalog query box over memory, per-agent diffs. It works identically on running and finished sessions. The bar it must clear: a colleague who has never seen lume explains a session they didn't run, unaided, in under five minutes.

**The REPL is how you *intervene and invent*.** `lume repl` attaches an nREPL to a session — live or dead — with the log, the memory connection, and the substrate's own functions in scope. This is the escape hatch that keeps harnesses honest and small:

- A gate is behaving strangely mid-run? Attach, query memory, inspect the mailbox, poke the flow state.
- Not sure a harness idea works? Don't write the EDN yet — drive the substrate functions by hand from the REPL (spawn one agent, run one verifier, route one message), and crystallize into a spec only what worked.
- The harness lacks a capability? Do that step manually at the REPL; the gap becomes the next backlog item.

Anything the interpreter can do, you can do interactively. Harness specs are, in effect, saved REPL sessions — which is why they can stay a one-page format without a trapdoor into general programming: the trapdoor is the REPL.

**Nothing touches the project.** All state — sessions, logs, memory, worktrees — lives under your home directory, keyed by repo. Working refs hide under `refs/lume/`, invisible to `git branch` and refused for push. The deliverable is ordinary commits; the audit trail stays home. `lume clean` plus a diff against a control clone proves the footprint is zero, and that proof is a CI test.

## What lume refuses to be (v1)

No daemon, server, or message bus — files and processes, until measured pain says otherwise (the log is the migration contract if that day comes; Kotlin is the pre-decided tool). No prompt library, planner, or RAG. No sandboxing beyond worktrees. No npm — the UI is server-rendered HTML plus in-browser Clojure (scittle), hackable with the same skills as everything else. No distribution; one machine.

## Building it on itself

The build is sequenced so each stage constructs the next — dogfooding as structure, not aspiration:

- **M0 — Log & replay** *(a weekend).* The event vocabulary, append-only log, terminal replay. Dogfood by hand: while building M1, manually log events describing your own work. It feels silly; it is the cheapest possible stress-test of the vocabulary, at the only moment revising it is free.
- **M1 — Spawn & verify** *(a week).* One agent, one worktree, one gate (the test suite), budgets. Done when a single-agent harness lands a real, test-gated commit in lume's own repo.
- **M2 — Routing, memory & REPL** *(a week).* Fan-out, mailboxes, Datalog memory, `lume repl`. Done when planner → workers → reviewer completes a real task — and when a new harness has been prototyped REPL-first before its spec existed.
- **M3 — The flip** *(ongoing).* Substrate development itself now goes through harnesses; out-of-harness changes need written justification. Every gap that forces you outside is the backlog, self-generated.
- **M4 — Web UI** *(built by harnesses).* Deliberately last: UI views are parallel, independently verifiable work — the ideal proving load for the self-hosted workflow. If lume can't build its own UI pleasantly, that is the most important bug report the project can produce.

Then the payoff milestone: hand an agent the harness schema and three examples, ask for a harness for a real task, `lume check`, `lume run`. Harnesses-as-data, closed loop.

## Success criteria

1. A new workflow's harness fits on one page and runs with one command.
2. Delete everything but the event log: replay, the UI (minus diffs), and the outcome still work.
3. The same Datalog query runs from the CLI, the REPL, the UI, and inside an agent.
4. Two consecutive substrate features land via harness runs; the UI is built entirely through them.
5. The footprint test passes in CI: harness run, merge, clean — the target repo is indistinguishable from a control.
6. No component but adapters knows which agent CLI is in play.

## Risks, briefly

Concurrent file writes → single-writer log, atomic renames. EDN sprawl → schemas for every event and spec, enforced by `lume check` in CI. Flow-DSL creep → new operations must subsume or retire an old one. UI creep → the no-build-step rule, and the UI stays a pure projection: missing state means a new event type, never a UI database. Early dogfooding drag → the flip only happens after M2 is real. And zero footprint is not zero disclosure — lume runs the agent CLIs you already run, as you; whether that's acceptable at work is a question about agent use, not about lume.

## Open questions (settle during M0–M2, in the REPL where possible)

1. Memory retraction, or superseding claims with provenance?
2. Merge strategy when fan-out workers overlap: interpreter-mediated, or a merger agent?
3. Per-agent budgets, or session-level only?
