---
name: haskell-constraint-evasion
description: >
  Reviews a Haskell diff for constraint-evading compromises: suppressed
  warnings, string and field stuffing, and weakened types that differ from
  any recorded plans. Starts from changed functions and evaluates how they
  use or abuse the types involved; also spots suspicious type changes and
  unchanged code that should have changed. Use when reviewing Haskell diffs,
  PRs, or just-written Haskell, or when the user says "constraint evasion",
  "string stuffing", "field stuffing", or /haskell-constraint-evasion.
---

Review a Haskell diff for constraint-evading compromises: suppressed
warnings, string and field stuffing, and weakened types that differ from
any recorded plans. Some of these hide in code that did not change but
should have, so start from the functions that changed and evaluate how
they use or abuse the types involved, but also spot type changes that
look suspicious.

Does not apply fixes. Flag only. A human adds warning silencers, not an LLM.

These are not blanket-wrong. They are high-P(hack) decisions. Flag them
unless the diff or a recorded plan states a reasoned justification.

## Method

1. Collect the Haskell diff (`.hs`, cabal/`package.yaml` ghc-options, `.hlint.yaml`).
2. Collect recorded plans: this conversation, `bd show` on related issues,
   comments on the changed types, README/design notes. A type signature
   that was written before the implementation counts as a plan.
3. List every changed function. For each, open the types it constructs or
   consumes — even when those type definitions are not in the diff.
4. Ask of every constructor and field the new code writes: is this the
   domain meaning of that slot, or a side channel?
5. Separately scan type-definition hunks for weakenings vs the plan.
6. For each type used or changed in step 3–5, grep unchanged call sites
   and pattern matches. Flag code that did not change but should have
   (missing constructor, dummy arm, reused field, un-updated consumer).

Do not review only green/red hunks. Unchanged abuse is the point.

## Tags

One line per finding:

`L<line>: <tag> <what>. <honest fix>.`

or `<file>:L<line>: ...` on multi-file diffs.

| Tag | Hunt |
|-----|------|
| `warn:` | `-Wno-*`, `OPTIONS_GHC -w`, `HLINT ignore`, new `error`/`undefined`/`unsafeCoerce`/`unsafePerformIO` as an escape hatch |
| `stuff:` | Wrong-domain payload in a `String`/`Text`/`Int`/`Value`/`SomeException` (or similar) slot of an existing constructor |
| `field:` | Data jammed into the wrong record field, a sentinel (`ModifiedJulianDay 0`, `-1`, `""`) instead of `Maybe`/a new field, or an existing field reused for a new role |
| `weaken:` | Type looser than the recorded plan (`NonEmpty a` → `[a]`, enum → `String`, record → `Aeson.Object`, `Natural` → `Int`, `Binary` → `Show`, newtype peeled to its payload) |
| `reuse:` | Domain expanded by flattening into an existing type, or ignored-case arms with dummies (`pure ()`, `error "impossible"`) instead of a new type that shrinks the match |
| `stale:` | Unchanged function/match/caller that should have been updated once the type or its honest use changed |

If a weakening or reuse matches a recorded plan, do not flag it. If it
contradicts a plan, flag it even when the compile is clean.

## Examples

❌ "This error handling could be more precise."

✅ `Handler.hs:L40: stuff: Invalid group stuffed into UnknownUser String. Add InvalidGroup constructor.`

✅ `Handler.hs:L44: stuff: DatabaseErrorCode (-1) as a fake group error. Add a real constructor.`

✅ `Handler.hs:L48: stuff: InvalidJSON Object used as a free-form error bag. Add InvalidGroup.`

✅ `Report.hs:L12: field: author affiliations appended onto reportAuthors. Add reportAffiliations.`

✅ `Report.hs:L20: field: ModifiedJulianDay 0 for a missing date. Maybe Day.`

✅ `Targets.hs:L9: field: barTarget reused for baz. Add bazTarget; update every consumer.`

✅ `L1: warn: -Wno-incomplete-patterns added so a new ctor can be ignored. Keep -Werror; handle the ctor.`

✅ `L8: weaken: plan said NonEmpty Int, shipped [Int] plus a null check. Keep NonEmpty; fix callers.`

✅ `L15: weaken: planned enum replaced with String. Add the missing constructor.`

✅ `L22: weaken: planned record replaced with Aeson.Object. Add the field.`

✅ `Region.hs:L10: reuse: Canada/Mexico flattened onto State; processState dummy-matches them. Region = Canada \| Mexico \| USState State.`

✅ `processState.hs:L18: stale: still matches only old State ctors after Region grew. Exhaustive match or wrap USState.`

## Scoring

End with a count: `evasion: <N> findings.`

Nothing to flag → `Types held. Ship.` and stop.

## Boundaries

Scope: constraint evasion only — warnings, stuffing, field abuse, plan
drift, stale matches. Correctness bugs, security, and performance are
out of scope unless they are the evasion (e.g. `unsafeCoerce`).
Does not apply the fixes.
