#!/usr/bin/env python3
"""Sweep nested_fit parameters on a benchmark case and record accuracy per likelihood call.

    python sweep.py test_12_gpu_crosscheck --grid "device=cpu,auto param1=0.5,1,2 accuracy=1e-5,1e-3" \
        --repeats 3 --threads 16 --out sweep_test12.tsv

Each run writes one TSV row: the parameters, log Z and its quoted uncertainty, likelihood
calls, iterations, wall time, the GPU log line, and the exit code. Runs are strictly
sequential so timings are not contaminated by each other. Plot with the notebook or the
dashboard: evidence error vs calls is the axis every change is judged on.
"""
import argparse
import itertools
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import time

YAML_KEYS = {  # parameter -> (regex on the input file, replacement template)
    "param1":     (r"(param1:\s*)[-\d.eE+]+",          r"\g<1>{v}"),
    "param2":     (r"(param2:\s*)[-\d.eE+]+",          r"\g<1>{v}"),
    "livepoints": (r"(livepoints:\s*)\d+",             r"\g<1>{v}"),
    "accuracy":   (r"(accuracy:\s*)[-\d.eE+]+",        r"\g<1>{v}"),
    "method":     (r"(method:\s*)(SLICE_SAMPLING\w*|RANDOM_WALK\w*|UNIFORM)", r"\g<1>{v}"),
    "max_tries":  (r"(max_tries:\s*)\d+",              r"\g<1>{v}"),
}
ENV_KEYS = {"device": "NESTED_FIT_DEVICE", "gpu_min_points": "NESTED_FIT_GPU_MIN_POINTS", "gpu_threads": "NESTED_FIT_GPU_THREADS",
            "gpu_precision": "NESTED_FIT_GPU_PRECISION"}
FIELDS = ["case", "repeat", "exit", "wall_s", "logZ", "uncert", "maxlnL", "calls", "iterations",
          "insertion_ks_p", "insertion_ks_p_first", "gpu"]


def insertion_index_test(diag_path, nlive):
    """Fowlie, Handley & Su 2020: the index at which each new point is inserted into the sorted
    live set must be uniform on 1..nlive. nested_fit stores it per discarded point as `rank` in
    nf_output_diag.dat. KS against uniform in chunks of nlive; returns the Bonferroni-corrected
    minimum p-value — small means the sampler is not producing independent constrained-prior
    draws (correlated chains, a bad step size, or a wrong GPU evaluation)."""
    import math
    try:
        ranks = [int(line.split()[1]) for line in open(diag_path) if line.strip() and not line.lstrip().startswith("#")]
    except (OSError, IndexError, ValueError):
        return None
    # The file holds every point's birth rank: the initial nlive (sorted from the prior, not
    # inserted) first, and the nlive survivors last - survivors are the points born at high
    # ranks, so they must be dropped or the test fails on survivorship alone.
    ranks = ranks[nlive:-nlive]
    if len(ranks) < 2 * nlive:
        return None
    chunks = [ranks[i:i + nlive] for i in range(0, len(ranks) - nlive + 1, nlive)]

    def ks_p(ch):
        xs = sorted(r / nlive for r in ch)
        n = len(xs)
        d = max(max(abs((i + 1) / n - x), abs(x - i / n)) for i, x in enumerate(xs))
        lam = (math.sqrt(n) + 0.12 + 0.11 / math.sqrt(n)) * d
        p = 2 * sum((-1) ** (k - 1) * math.exp(-2 * k * k * lam * lam) for k in range(1, 101))
        return max(0.0, min(1.0, p))

    # The first ~2K insertions after the prior phase deviate reproducibly (deficit of high
    # ranks, at K = 300 and K = 2048 alike); they are reported on their own so the
    # main-phase verdict is not driven by them.
    early = min(ks_p(ch) for ch in chunks[:2]) * min(2, len(chunks))
    rest = chunks[2:]
    main = min(ks_p(ch) for ch in rest) * len(rest) if rest else None
    return (min(1.0, main) if main is not None else None, min(1.0, early))


def parse_grid(spec):
    grid = {}
    for item in spec.split():
        k, vs = item.split("=", 1)
        grid[k] = vs.split(",")
    return grid


def find_binary():
    here = pathlib.Path(__file__).resolve().parent.parent
    cands = sorted(here.glob("bin/nested_fit[0-9]*")) + sorted(pathlib.Path.home().glob("bin/nested_fit[0-9]*"))
    cands = [c for c in cands if c.is_file() and os.access(c, os.X_OK) and not c.name.endswith(".so")]
    if not cands:
        sys.exit("no nested_fit binary in bin/ or ~/bin")
    return cands[-1]


def run_one(case_dir, params, threads, binary, timeout):
    work = pathlib.Path(tempfile.mkdtemp(prefix="nf_sweep_"))
    for f in case_dir.iterdir():
        if f.is_file():
            shutil.copy(f, work / f.name)
    text = (work / "nf_input.yaml").read_text()
    for k, v in params.items():
        if k in YAML_KEYS:
            pat, rep = YAML_KEYS[k]
            text, n = re.subn(pat, rep.format(v=v), text)
            if n == 0:
                sys.exit(f"{k}: key not found in {case_dir}/nf_input.yaml")
    (work / "nf_input.yaml").write_text(text)
    env = os.environ.copy()
    env.update({"OMP_NUM_THREADS": str(threads), "OMP_WAIT_POLICY": "passive"})
    for k, v in params.items():
        if k in ENV_KEYS:
            env[ENV_KEYS[k]] = str(v)
    t0 = time.perf_counter()
    try:
        p = subprocess.run([str(binary), "-c"], cwd=work, env=env, capture_output=True, text=True, timeout=timeout)
        rc, log = p.returncode, p.stdout + p.stderr
    except subprocess.TimeoutExpired as e:
        rc, log = 124, (e.stdout or "") + (e.stderr or "")
    wall = time.perf_counter() - t0
    g = lambda pat: (re.search(pat, log) or [None, None])[1]
    row = {
        "exit": rc, "wall_s": f"{wall:.1f}",
        "logZ": g(r"Final evidence \(log\):\s*([-\d.E+]+)"),
        "uncert": g(r"Evidence estimated uncertainty \(log\):\s*([-\d.E+]+)"),
        "maxlnL": g(r"Max likelihood \(log\):\s*([-\d.E+]+)"),
        "calls": (re.findall(r"\d+", (re.search(r"likelihood calls[^\n]*", log) or [""])[0].split(":")[-1]) or [None])[-1],
        "iterations": g(r"N\. of total iteration:\s*(\d+)"),
        "gpu": re.sub(r"\x1b\[[0-9;]*m", "", g(r"(GPU: [^\n]*)") or "").strip(),
    }
    nlive = re.search(r"livepoints:\s*(\d+)", text)
    ks = insertion_index_test(work / "nf_output_diag.dat", int(nlive.group(1))) if nlive else None
    row["insertion_ks_p"] = f"{ks[0]:.3g}" if ks and ks[0] is not None else ""
    row["insertion_ks_p_first"] = f"{ks[1]:.3g}" if ks else ""
    # error evolution: (step, running log Z, remaining-evidence estimate Ea), one row per progress line
    row["_progress"] = [(int(m.group(1)), float(m.group(2)), float(m.group(3)))
                        for m in re.finditer(r"\| S:\s*(\d+) \|.*?\| E:\s*([-\d.E+]+) \|.*?\| Ea:\s*([\d.E+-]+)", log)]
    shutil.rmtree(work, ignore_errors=True)
    return row


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("case", help="benchmark directory (contains nf_input.yaml and the data)")
    ap.add_argument("--grid", required=True, help='e.g. "device=cpu,auto param1=0.5,1,2 accuracy=1e-5,1e-3"')
    ap.add_argument("--repeats", type=int, default=2)
    ap.add_argument("--threads", type=int, default=os.cpu_count() or 8)
    ap.add_argument("--timeout", type=int, default=3600)
    ap.add_argument("--out", default="sweep.tsv")
    a = ap.parse_args()

    case_dir = pathlib.Path(a.case).resolve()
    grid = parse_grid(a.grid)
    keys = list(grid)
    binary = find_binary()
    out = pathlib.Path(a.out)
    progress_dir = out.with_suffix(".progress")   # one JSON per run: the error-evolution trace
    progress_dir.mkdir(parents=True, exist_ok=True)
    new = not out.exists()
    with out.open("a") as f:
        if new:
            f.write("\t".join(keys + FIELDS) + "\n")
        for values in itertools.product(*grid.values()):
            params = dict(zip(keys, values))
            for r in range(1, a.repeats + 1):
                row = run_one(case_dir, params, a.threads, binary, a.timeout)
                row.update({"case": case_dir.name, "repeat": r})
                f.write("\t".join(str(params[k]) for k in keys) + "\t" + "\t".join(str(row[k]) for k in FIELDS) + "\n")
                f.flush()
                tag = "_".join(f"{k}={params[k]}" for k in keys) + f"_rep{r}"
                (progress_dir / f"{tag}.json").write_text(json.dumps({"params": params, "repeat": r, "progress": row["_progress"]}))
                print(f"{params}  rep{r}  exit={row['exit']} wall={row['wall_s']}s logZ={row['logZ']} ±{row['uncert']} calls={row['calls']}  {row['gpu']}")


if __name__ == "__main__":
    main()
