# SPDX-License-Identifier: MPL-2.0
#
# Classify one issue title against the estate label taxonomy.
#
#   jq -r --arg title "docs: fix the README" \
#         --argjson have '[]' \
#         -f .github/scripts/classify-issue.jq .github/label-classifier.json
#
# Prints one label per line, or NOTHING when it cannot place the issue
# confidently. Nothing printed means "leave it for a human" -- a correct
# outcome, not a failure.
#
# WHY jq AND NOT PYTHON
#
#   Python is fully banned estate-wide: the `governance / Language / package
#   anti-pattern policy` gate runs `git ls-files '*.py'` and fails the PR
#   ("Python is fully banned -- use AffineScript/Rust/SPARK/Julia"). This file
#   is dispatched into every repo in the estate, so shipping it as .py would
#   mean shipping an exemption into every repo too -- normalising the policy
#   away by sweep. jq is preinstalled on every GitHub runner, is not on the
#   banned list, needs no action (so no actions.lock entry can drift), and the
#   rules are already JSON.
#
#   The canonical implementation remains scripts/label-classify.py in the hub,
#   which never runs in CI. tests/test-classifier-parity.py asserts this file
#   agrees with it on every title in the corpus.
#
# `$have` lists labels the issue already carries. Anything already present is
# never re-suggested, and the classifier stays out of any max-1 tier the issue
# already has a label in, so a human's classification is never overridden.

# Escape every non-alphanumeric so a keyword is matched literally. Escaping
# punctuation that needs no escape is harmless in Oniguruma.
def reesc: gsub("(?<c>[^A-Za-z0-9 _])"; "\\\(.c)");

def norm: (. // "") | ascii_downcase
        | sub("^[[:space:]]+"; "") | sub("[[:space:]]+$"; "");

# Asymmetric boundary: STRICT on the left, inflection-tolerant on the right.
#
# Measured over the issue corpus, the two error directions are not symmetric:
#   * every false positive is a LEFT-side prefix -- `lean` in "clean up",
#     `abi` in "capability", `mpl` in "Implement", `ffi` in "AffineScript",
#     `smt` in "wasmtime". The left boundary must stay strict.
#   * every real miss is a RIGHT-side inflection -- `test` vs "tests",
#     `theorem` vs "theorems", `todo` vs "TODOs", `scaffold` vs "scaffolding".
#
# The right side therefore admits a CLOSED set of inflections. Closed, not open
# (`.*`), because an open right side re-admits the prefix false positives.
#
# `ion`/`ation` are excluded from the base set: they mint unrelated words
# (`port` + `ion` = "portion", and `port` is a live keyword). They are enabled
# only for shapes that are unambiguously truncated stems -- `-at`
# (instantiat, investigat, adjudicat) and `-ment` (document, implement).
def kwrx($kw):
  ( "s|es|ed|d|ing|er|ers|y|ies"
    + (if   ($kw | endswith("at"))   then "|ion|ions|e"
       elif ($kw | endswith("ment")) then "|ation|ations"
       else "" end)
  ) as $suf
  # Boundaries are conditional: a keyword not starting alphanumeric has no left
  # boundary to enforce, and one not ending alphanumeric takes no suffix.
  | (if ($kw | test("^[A-Za-z0-9]")) then "(?<![A-Za-z0-9])" else "" end)
  + ($kw | reesc)
  + (if ($kw | test("[A-Za-z0-9]$"))
     then "(?:" + $suf + ")?(?![A-Za-z0-9])" else "" end);

def kwhit($kw; $text): $text | test(kwrx($kw); "i");

# Merge one rule object's labels into a flat array.
def rulelabels:
  ([.type?, .meta?, .scope?, .priority?] + (.areas? // []))
  | map(select(. != null));

# Leading `[tag]`, stripped so a following prefix can also match.
def bracket($R; $t):
  (($t | capture("^[[:space:]]*\\[(?<tag>[^\\]]{1,25})\\]")) // null) as $m
  | if $m == null then {rule: null, rest: $t}
    else (($m.tag | norm | split("#")[0]) | norm) as $tag
       | { rule: ($R.bracket_tag[$tag] // null),
           rest: ($t | sub("^[[:space:]]*\\[[^\\]]{1,25}\\]"; "")) }
    end;

# Leading `word:` / `word(scope):` conventional-commit prefix.
def prefixrule($R; $t):
  (($t | capture("^[[:space:]]*(?<w>[A-Za-z][A-Za-z0-9_./-]{1,24})(?:[[:space:]]*\\([^)]*\\))?[[:space:]]*:")) // null) as $m
  | if $m == null then null
    else ($m.w | norm) as $k
       # Compound prefixes such as "adaptive/must:" carry their meaning in the
       # ISO 14764 category only; the modality does not label.
       | (if ($R.prefix_split_on // "") != "" and ($k | contains($R.prefix_split_on))
          then ($k | split($R.prefix_split_on) | .[0]) else $k end) as $key
       | ($R.title_prefix[$key] // null)
    end;

def signals($R; $tl; $sec):
  [ ($R[$sec] // {}) | to_entries[]
    | select(.value | any(. as $k | kwhit($k; $tl)))
    | .key ];

# The HIGHEST-PRECEDENCE matching type, not merely the first in key order.
def kwtype($R; $tl):
  [ $R.keyword_type | to_entries[]
    | select(.value | any(. as $k | kwhit($k; $tl)))
    | .key ]
  | if length == 0 then null
    else min_by([($R.precedence[.] // 99), .]) end;

# Drop violations of each tier's `max`, keeping the highest-precedence member.
def enforce($R; $labels):
  ($labels | unique)
  | group_by($R.tier_of[.] // "?")
  | map( ($R.tier_of[.[0]] // "?") as $tier
       | ($R.tier_max[$tier] // null) as $mx
       | if $mx == null or (length <= $mx) then .
         else (sort_by([($R.precedence[.] // 99), .]))[0:$mx] end )
  | flatten;

def classify($R; $title; $have0):
  ($title // "")                                   as $t0
  | ($t0 | norm)                                   as $tl
  | ($have0 | map(select(. != null and . != ""))
            | unique)                              as $have
  | ($R.tier_of | keys)                            as $canon
  | $R.types                                       as $types
  | bracket($R; $t0)                               as $b
  | (if $b.rule != null then ($b.rule | rulelabels) else [] end)  as $l1
  | prefixrule($R; $b.rest)                        as $pr
  | (if $pr != null then ($pr | rulelabels) else [] end)          as $l2
  | (($b.rule != null) or ($pr != null))           as $matched0
  # 3. keyword areas are additive and never contribute a type
  | ($l1 + $l2 + signals($R; $tl; "keyword_area")) as $acc
  # 4. a type only if neither the rules nor the issue already supplied one
  | (if (($acc + $have) | any(. as $x | $types | index($x)))
     then null else kwtype($R; $tl) end)           as $ty
  | ($acc + (if $ty != null then [$ty] else [] end))             as $acc
  | ($matched0 or ($ty != null))                   as $matched
  | ( $acc
      + signals($R; $tl; "status_signal")
      + signals($R; $tl; "meta_signal")
      + signals($R; $tl; "scope_signal") )        as $acc
  # NOTE: `frozen` is deliberately NOT subtracted. Frozen means "never rename or
  # delete this label" -- `security` is frozen because triage.yml pins it in
  # exempt-issue-labels. APPLYING it to an issue is correct; only the
  # definition is protected.
  | ($acc | map(select(. as $x | $canon | index($x))) | unique) as $acc
  | enforce($R; $acc + ($have | map(select(. as $x | $canon | index($x)))))  as $acc
  | ($acc - $have)                                 as $out
  # Stay out of any max-1 tier the issue ALREADY has a label in -- a human's,
  # or one an ISSUE_TEMPLATE applied. A prefix rule fires unconditionally, so
  # "fix: ..." on an issue already labelled `enhancement` would otherwise add
  # `bug` beside it. This covers every max-1 tier (type, priority, status,
  # meta, scope), not just type.
  | ( [ $R.tier_max | to_entries[] | select(.value == 1) | .key ]
      | map(. as $t | select($have | any(($R.tier_of[.] // "?") == $t)))
    )                                              as $lockedtiers
  | ($out | map(select(($R.tier_of[.] // "?") as $t | ($lockedtiers | index($t)) | not))) as $out
  # A rule must actually have FIRED: keyword-area hits alone are not enough.
  | if ($matched | not) then []
    # a type is mandatory
    elif ((($out + $have) | any(. as $x | $types | index($x))) | not) then []
    else ($out | sort) end;

classify(.; $title; $have) | .[]
