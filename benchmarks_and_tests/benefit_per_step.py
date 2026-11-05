#!/usr/bin/env python3
"""The benefit of each proposed step, quantified from the matrix and the dedicated runs.

    python benefit_per_step.py matrix.tsv

Prints (1) the suite: per lever, median and best speed-up over the git version and the evidence
agreement; (2) Gonçalo's case: the stacked steps from the git version on his cores to the full
configuration on his A4000, with the time of one full run (570 000 steps, 2.9e8 evaluations)."""
import math
import re
import sys

import pandas as pd

FULL_RUN_EVALS = 2.9e8          # 570 000 steps x ~510 calls per step, from the converged K=200 run (114 nats)
SINGLE_PHASE = 0.29             # fraction of the steps before the precision switch (step 6 000-6 600 of 22 800)
CPU48 = 100.0                   # his 48 cores, evaluations per second (0.5 s per call per core, perfect scaling)
WINGS_MIXED = 954.0             # per-term rate with the asymptotic Faddeeva series in the wings (A4000, 8 threads, 4 Nov.)

d = pd.read_csv(sys.argv[1], sep="\t", dtype=str)
for c in ("wall_s", "logZ", "uncert", "calls", "steps"):
    d[c] = pd.to_numeric(d[c], errors="coerce")

print("## The suite (every example of the repository), one run per cell, against nightly 5.7.1\n")
print("| step | cases | median speed-up | best | evidence within 1.5 σ (pairs with both) | runs without evidence |")
print("|---|---|---|---|---|---|")
for l in ["branch-cpu", "auto", "gpu", "width1", "gpu+width1", "gpu+adaptive", "gpu+mixed", "gpu+split2", "gpu+all"]:
    gains, agree, pairs, noz, n = [], 0, 0, 0, 0
    for case, g in d.groupby("case", sort=False):
        if case.startswith("/"):
            continue
        b = g[g.lever == "baseline"]; r = g[g.lever == l]
        if b.empty or r.empty:
            continue
        b, r = b.iloc[0], r.iloc[0]
        if str(r.exit) != "0" or not (r.wall_s > 0):
            continue
        n += 1; gains.append(b.wall_s / r.wall_s)
        if math.isnan(r.logZ):
            noz += 1
        elif not math.isnan(b.logZ):
            pairs += 1; agree += abs(r.logZ - b.logZ) / (math.hypot(b.uncert or 0, r.uncert or 0) or 1) <= 1.5
    if gains:
        gains.sort(); print(f"| {l} | {n} | {gains[len(gains)//2]:.2f}× | {gains[-1]:.2f}× | {agree} / {pairs} | {noz} |")

g = d[d.case.str.startswith("/")]
if not g.empty:
    print("\n## Gonçalo's case on the A4000 box (K = 200, 8 threads, 300 steps or a 2 400 s cap)\n")
    print("| step | steps | wall (s) | s per step | evaluations / s on the device |")
    print("|---|---|---|---|---|")
    for _, r in g.iterrows():
        m = re.search(r"(\d+) likelihood evaluations", str(r.gpu))
        rate = f"{int(m.group(1)) / r.wall_s:.0f}" if m and r.wall_s > 0 else "CPU"
        if str(r.exit) == "124":   # timed out; the step counter prints every 100 steps, so steps is a floor
            print(f"| {r.lever} | {r.steps:.0f}-{r.steps + 99:.0f} (timeout) | {r.wall_s:.0f} | {r.wall_s / (r.steps + 99):.0f}-{r.wall_s / r.steps:.0f} | {rate} |")
        else:
            print(f"| {r.lever} | {r.steps:.0f} | {r.wall_s:.0f} | {r.wall_s / r.steps:.1f} | {rate} |")

def device_rate(lever):
    # the runner keeps the first 1 000 characters of the log lines; when the stats line is cut
    # (adaptive runs log more), the evaluation count is the median of the other GPU rows
    r = g[g.lever == lever].iloc[0]
    m = re.search(r"(\d+) likelihood evaluations", str(r.gpu))
    counts = [int(x) for x in g.gpu.astype(str).str.extract(r"(\d+) likelihood evaluations")[0].dropna()]
    n = int(m.group(1)) if m else int(sorted(counts)[len(counts) // 2])
    return n / r.wall_s, n

if not g.empty:
    # measured on the A4000 box: calls per step with width 1, device rates per precision
    _, n_gpu = device_rate("gpu"); _, n_w1 = device_rate("gpu+width1")
    W1 = n_gpu / n_w1
    R_DOUBLE_8, _ = device_rate("gpu")            # 8 GPU threads
    R_DOUBLE_2, _ = device_rate("gpu+split2")     # 2 GPU threads: the A4000 serves 2 streams better than 8
    R_SINGLE, _ = device_rate("gpu+adaptive")
    R_MIXED, _ = device_rate("gpu+mixed")
else:
    W1, R_DOUBLE_8, R_DOUBLE_2, R_SINGLE, R_MIXED = 1.4, 83, 108, 2400, 789
EV = FULL_RUN_EVALS / W1

print("\n## Gonçalo's full run, stacked (rates measured in the sampler on the A4000 box, evaluations per second)\n")
steps = [
    ("git version on his 48 cores", FULL_RUN_EVALS, CPU48, None),
    (f"+ slice width 1 ({W1:.2f}× fewer calls, measured on his model)", EV, CPU48, None),
    (f"+ his A4000 alone, double, 8 GPU threads ({R_DOUBLE_8:.0f}/s: the card equals the cores)", EV, R_DOUBLE_8, None),
    (f"+ 2 GPU threads and the 46 other cores on the CPU ({R_DOUBLE_2:.0f} + {CPU48:.0f}/s)", EV, R_DOUBLE_2 + CPU48, None),
    (f"+ adaptive precision: single for the first {SINGLE_PHASE:.0%} of the steps ({R_SINGLE:.0f}/s)", EV, (R_SINGLE, R_DOUBLE_2 + CPU48), SINGLE_PHASE),
    (f"+ per-term precision in the tail ({R_MIXED:.0f}/s, 8 GPU threads, + the cores)", EV, (R_SINGLE, R_MIXED + CPU48), SINGLE_PHASE),
]
print("| configuration | rate (evals / s) | one full run | gain over the git version |")
print("|---|---|---|---|")
t0 = None
for name, ev, rate, frac in steps:
    if frac is None:
        t = ev / rate; rtxt = f"{rate:.0f}"
    else:
        t = ev * (frac / rate[0] + (1 - frac) / rate[1]); rtxt = f"{rate[0]:.0f} then {rate[1]:.0f}"
    t0 = t0 or t
    print(f"| {name} | {rtxt} | {t/86400:.1f} days | {t0/t:.1f}× |")
print(f"\nWithout a single phase (the switch tripped at step 100 in the converged run): "
      f"{EV / (R_MIXED + CPU48) / 86400:.1f} days, {FULL_RUN_EVALS / CPU48 / (EV / (R_MIXED + CPU48)):.1f}×.")
print(f"With the wing series in the per-term tail ({WINGS_MIXED:.0f}/s, 1.21× in the sampler): "
      f"{EV / (WINGS_MIXED + CPU48) / 86400:.1f} days without a single phase ({FULL_RUN_EVALS / CPU48 / (EV / (WINGS_MIXED + CPU48)):.1f}×), "
      f"{EV * (SINGLE_PHASE / R_SINGLE + (1 - SINGLE_PHASE) / (WINGS_MIXED + CPU48)) / 86400:.1f} days with one.")
