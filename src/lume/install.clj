(ns lume.install
  "Install: set up the agent side of this machine. Safe to re-run; writes only
  what differs and nothing inside any repo."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [lume.kernel :refer [*default-agent*]]))

;; A user-level skill that teaches any agent session to work through lume. It names
;; no agent: lume.agents/setup says where each agent keeps it.
(def skill
  "---
name: lume
description: Work in a repo through lume, the durable log for long agent tasks. Use when a repo has a lume session (`lume status` prints one), or when asked to plan, drive, or hand off work with lume.
---

# Working through lume

Lume keeps a long task's state in an event log, not in your context. `lume status`
is the handoff: there is no handoff file, and the next session starts from it too.

## Start

Run `lume status` first. It is one screen:
- goal: what the session is for
- asks: questions waiting for the human; don't redo work they block
- pinned notes: standing lessons; follow them
- open items: the backlog, each with the gate that decides done, and its latest
  measurement row if it has one (`lume measure <id>` runs it again; `lume ledger [<id>]`
  lists every row, closed items' too, each against the row it was compared with).
  A measured item stays open after a spawn, gate passed or not: compare the row
  with the one it names (vs mN) and close it done to keep the change, dropped to roll back
- recent notes and events: what the last session did

`lume item show <id>` prints an item's full intent, gate and history when status clips it;
`lume ask show <id>` does the same for an ask (who asked, its item, its answer).
`lume ls` lists the repo's sessions, newest first; `lume status <session>` and
`lume log --session <session>` read a past one without touching it. `lume replay`
lists a session's decisions (drive turns, spawns), each with its event number N;
`lume replay --at N` is status as it stood then, with what that decision did.
To prototype a fold, a query or a screen, run `lume repl [<session>]` in the background:
it prints a port, and `clj-nrepl-eval -p <port> \"<code>\"` evaluates with the session's
`events`, `state`, `(render)` (`(render N)`: after event N), `(refresh!)` and lume.* in scope.
Try the code on the real log there before writing it; the repl only logs that it opened.
`(measure \"<cmd>\" {:runs N :drop K})` there runs a candidate measurement exactly as a
spec's would (sh -c in the repo, name=number values, wall time) and returns the row unlogged. `lume clean`
deletes sessions and is the human's; never run it.
If there is no session, don't start one unprompted; ask the human for the goal.

## Work

- Add work as an item, intent first, then its gate:
  `lume item add \"<what and why>\" --gate \"<cmd>\"`.
  The gate is a command whose exit 0 means done (e.g. the repo's tests). An ungated
  item can't close itself when its worker exits.
- `lume spawn <id>` runs a worker on an item; `lume drive` runs driver turns, each
  making one decision, until an ask is open, a budget limit is reached, or nothing
  is left. `lume budget` shows spend; you may lower a limit, never raise one.
- `lume cadence` says when drive hands back: how many spawns a driver turn may
  request (`batch N`, run in order), whether a failed gate ends the batch
  (`on gate-fail`), and when a stalled measurement asks the human (`on plateau K`).
  Lower is better for a measured number unless `lume item measure` names it with
  `--higher <name>` (a score); rows mark those `name↑`. Rows after a spawn compare
  (`vs mN`) with its before row, the latest row before it; `--before` has each spawn
  take one first. You may hand back more often than the human's cadence, never less.
- `lume spec` prints the session's harness (goal, gate, allow, budget, cadence,
  items) as one page of EDN; `lume check <spec.edn>` validates a spec file, and
  `lume run <spec.edn>` starts a session from one, baselines it and drives
  (`--turns 0` doesn't drive). A spec with `:allow` or a cadence above
  every-spawn is the human's to run.
- After a worker: review its diff, commit it with a message saying what and why, then
  `lume note \"committed <id>: <change id> <what>\"`.
- `lume item close <id> done \"<why>\"` runs the gate and is refused if it fails;
  `dropped` needs only a reason and changes nothing; `dropped --revert \"<why>\"` also
  restores the working copy to the item's before row (its jj commit, or git commit
  without jj) and is refused if there is none.
- Record what you learn with `lume note \"<text>\"`; `lume note --pin` for anything the
  next session must not forget. When status lacked something you needed, add an item.

## The human

- When only the human can decide, `lume ask \"<question>\"` and stop that line of work.
  Keep the question to one or two lines: the question, lettered options, and your
  recommendation, e.g. `lume ask \"i45 plateaued: (a) keep going (b) try X (c) stop? I'd pick c; see n120\"`.
  Put the context in a `lume note` first and name it (n120) in the question.
- Never run `lume allow` or `lume answer`: permissions and answers are the human's,
  and lume refuses them from agents anyway: they need a terminal and a typed confirmation.
- Never push. Commit locally; pushing is up to the human.
- Lume writes nothing in the repo; its state lives under ~/.lume. Don't read or edit
  it directly; use the commands.
")

(defn install [[a b]]
  (let [agent (keyword (if (= "--agent" a) b *default-agent*))
        {:keys [files manual]} ((requiring-resolve 'lume.agents/setup) agent skill)]
    (str/join "\n"
              (concat
               (for [[path content] files]
                 (let [old (when (fs/exists? path) (slurp path))]
                   (if (= old content)
                     (str "unchanged " path)
                     (do (fs/create-dirs (fs/parent path))
                         (spit path content)
                         (str (if old "updated   " "wrote     ") path)))))
               manual))))

(def feature
  {:name "install"
   :commands [{:path ["install"] :needs :none :usage "usage: lume install [--agent claude|claude-acp|devin]"
               :run (fn [_ args] (install args))
               :help "lume install [--agent A]                    set up agents on this machine (a lume skill); safe to re-run.
                                            Agents: claude, claude-acp, devin; LUME_AGENT sets the default (claude)"}]})
