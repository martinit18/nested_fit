#!/usr/bin/env python3
"""matrix.tsv -> a markdown table: per case, the baseline (git version) and, per lever, the speed-up and the evidence agreement."""
import math
import sys

import pandas as pd

d = pd.read_csv(sys.argv[1], sep="\t", dtype=str)
for c in ("wall_s", "logZ", "uncert", "calls", "steps"):
    d[c] = pd.to_numeric(d[c], errors="coerce")
levers = [l for l in ["branch-cpu", "auto", "gpu", "width1", "gpu+width1", "gpu+adaptive", "gpu+mixed", "gpu+split2", "gpu+all"] if l in set(d.lever)]
print("| case | git version: wall, log Z | " + " | ".join(levers) + " |")
print("|---|---|" + "---|" * len(levers))
for case, g in d.groupby("case", sort=False):
    b = g[g.lever == "baseline"]
    if b.empty:
        continue
    b = b.iloc[0]
    cells = []
    for l in levers:
        r = g[g.lever == l]
        if r.empty:
            cells.append("—"); continue
        r = r.iloc[0]
        if r.exit != "0" and r.exit != 0:
            cells.append(f"exit {r.exit}"); continue
        gain = b.wall_s / r.wall_s if r.wall_s and r.wall_s > 0 else float("nan")
        if not math.isnan(b.logZ) and not math.isnan(r.logZ):
            sig = math.hypot(b.uncert or 0, r.uncert or 0) or 1.0
            z = abs(r.logZ - b.logZ) / sig
            cells.append(f"**{gain:.2f}×**, {z:.1f} σ" if z <= 1.5 else f"{gain:.2f}×, ⚠ {z:.1f} σ")
        else:
            cells.append(f"{gain:.2f}× (steps {int(r.steps) if not math.isnan(r.steps) else '?'})")
    base = f"{b.wall_s:.0f} s" + (f", {b.logZ:.3f} ± {b.uncert:.3f}" if not math.isnan(b.logZ) else f" ({int(b.steps) if not math.isnan(b.steps) else '?'} steps)")
    print(f"| `{case}` | {base} | " + " | ".join(cells) + " |")

# per-lever summary across cases: median speed-up where the lever applies, and how many cases keep the evidence
print()
print("| lever | cases | median speed-up | best | evidence within 1.5 σ of the git version |")
print("|---|---|---|---|---|")
for l in levers:
    gains, agree, n = [], 0, 0
    for case, g in d.groupby("case", sort=False):
        b = g[g.lever == "baseline"]; r = g[g.lever == l]
        if b.empty or r.empty or str(r.iloc[0].exit) != "0":
            continue
        b, r = b.iloc[0], r.iloc[0]
        if not (r.wall_s and r.wall_s > 0):
            continue
        n += 1; gains.append(b.wall_s / r.wall_s)
        if not math.isnan(b.logZ) and not math.isnan(r.logZ):
            sig = math.hypot(b.uncert or 0, r.uncert or 0) or 1.0
            agree += abs(r.logZ - b.logZ) / sig <= 1.5
    if gains:
        gains.sort()
        print(f"| {l} | {n} | {gains[len(gains)//2]:.2f}× | {gains[-1]:.2f}× | {agree} / {n} |")
