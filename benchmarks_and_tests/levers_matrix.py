#!/usr/bin/env python3
"""Every case x every lever, against the current git version.

    python levers_matrix.py --baseline /root/nf_nightly/bin/nested_fit5.7.1 --branch /root/nf_gpu/bin/nested_fit5.6.10 \
        --cases cases.txt --out matrix.tsv --threads 8

cases.txt: one case per line, a directory (absolute, or relative to the repo root given by --repo)
followed by optional yaml overrides, e.g. `/root/case livepoints=200 max_steps=500 hard_writing=false`.
Levers (one row each per case): baseline (the git binary), branch-cpu (the branch on the CPU: no
regression), gpu, width1 (slice width 1, SLICE_SAMPLING cases only), gpu+width1, gpu+adaptive.
Each row: exit, wall, log Z, its uncertainty, max log L, likelihood calls, the GPU log lines.
"""
import argparse
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import time

YAML_KEYS = {
    "param1":       (r"(param1:\s*)[-\d.eE+]+",          r"\g<1>{v}"),
    "param2":       (r"(param2:\s*)[-\d.eE+]+",          r"\g<1>{v}"),
    "livepoints":   (r"(livepoints:\s*)\d+",             r"\g<1>{v}"),
    "accuracy":     (r"(accuracy:\s*)[-\d.eE+]+",        r"\g<1>{v}"),
    "max_steps":    (r"(max_steps:\s*)\d+",              r"\g<1>{v}"),
    "max_tries":    (r"(max_tries:\s*)\d+",              r"\g<1>{v}"),
    "hard_writing": (r"(hard_writing:\s*)\w+",           r"\g<1>{v}"),
}
LEVERS = {   # name -> (binary, env, yaml overrides); "cuda" forces the card, "auto" lets the install decide
    "baseline":     ("baseline", {},                                                    {}),
    "branch-cpu":   ("branch",   {"NESTED_FIT_DEVICE": "cpu"},                          {}),
    "auto":         ("branch",   {"NESTED_FIT_DEVICE": "auto"},                         {}),
    "gpu":          ("branch",   {"NESTED_FIT_DEVICE": "cuda"},                         {}),
    "width1":       ("branch",   {"NESTED_FIT_DEVICE": "cpu"},                          {"param1": "1"}),
    "gpu+width1":   ("branch",   {"NESTED_FIT_DEVICE": "cuda"},                         {"param1": "1"}),
    "gpu+adaptive": ("branch",   {"NESTED_FIT_DEVICE": "cuda", "NESTED_FIT_GPU_PRECISION": "adaptive"}, {}),
    "gpu+mixed":    ("branch",   {"NESTED_FIT_DEVICE": "cuda", "NESTED_FIT_GPU_PRECISION": "mixed"},    {}),
    "gpu+split2":   ("branch",   {"NESTED_FIT_DEVICE": "cuda", "NESTED_FIT_GPU_THREADS": "2"},          {}),
    "gpu+all":      ("branch",   {"NESTED_FIT_DEVICE": "cuda", "NESTED_FIT_GPU_PRECISION": "adaptive", "NESTED_FIT_GPU_THREADS": "2"}, {"param1": "1"}),
}
FIELDS = ["case", "lever", "exit", "wall_s", "logZ", "uncert", "maxlnL", "calls", "steps", "gpu"]


def apply_overrides(text, overrides):
    for k, v in overrides.items():
        pat, rep = YAML_KEYS[k]
        text, n = re.subn(pat, rep.format(v=v), text)
        if n == 0:
            return None
    return text


def run_one(case_dir, binary, env_extra, overrides, threads, timeout):
    work = pathlib.Path(tempfile.mkdtemp(prefix="nf_matrix_"))
    shutil.copytree(case_dir, work, dirs_exist_ok=True)
    for f in work.rglob("nf_output_*"):
        f.unlink()
    text = apply_overrides((work / "nf_input.yaml").read_text(), overrides)
    if text is None:
        shutil.rmtree(work, ignore_errors=True)
        return None
    (work / "nf_input.yaml").write_text(text)
    env = os.environ.copy()
    env.update({"OMP_NUM_THREADS": str(threads), "OMP_WAIT_POLICY": "passive", "TRACY_NO_INVARIANT_CHECK": "1"})
    env.update(env_extra)
    t0 = time.perf_counter()
    try:
        p = subprocess.run([str(binary), "-c"], cwd=work, env=env, capture_output=True, timeout=timeout)
        rc, log = p.returncode, (p.stdout + p.stderr).decode("utf-8", errors="replace")   # some examples print Latin-1
    except subprocess.TimeoutExpired as e:
        rc, log = 124, ((e.stdout or b"") + (e.stderr or b"")).decode("utf-8", errors="replace")
    wall = time.perf_counter() - t0
    log = re.sub(r"\x1b\[[0-9;]*m", "", log)
    g = lambda pat: (re.search(pat, log) or [None, None])[1]
    steps = re.findall(r"\| S:\s*(\d+) \|", log)
    row = {
        "exit": rc, "wall_s": f"{wall:.1f}",
        "logZ": g(r"Final evidence \(log\):\s*([-\d.E+]+)"),
        "uncert": g(r"Evidence estimated uncertainty \(log\):\s*([-\d.E+]+)"),
        "maxlnL": g(r"Max likelihood \(log\):\s*([-\d.E+]+)"),
        "calls": (re.findall(r"\d+", (re.search(r"likelihood calls[^\n]*", log) or [""])[0].split(":")[-1]) or [None])[-1],
        "steps": steps[-1] if steps else g(r"Maximum number of iteraction reached\s*=\s*(\d+)"),
        "gpu": " | ".join(l.strip() for l in re.findall(r"GPU: [^\n]*", log))[:400],
    }
    shutil.rmtree(work, ignore_errors=True)
    return row


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--repo", default=".")
    ap.add_argument("--baseline", required=True)
    ap.add_argument("--branch", required=True)
    ap.add_argument("--cases", required=True)
    ap.add_argument("--levers", default=",".join(LEVERS))
    ap.add_argument("--threads", type=int, default=8)
    ap.add_argument("--timeout", type=int, default=3600)
    ap.add_argument("--out", default="matrix.tsv")
    a = ap.parse_args()
    binaries = {"baseline": pathlib.Path(a.baseline).resolve(), "branch": pathlib.Path(a.branch).resolve()}
    out = pathlib.Path(a.out)
    new = not out.exists()
    done = set()
    if not new:
        done = {tuple(l.split("\t")[:2]) for l in out.read_text().splitlines()[1:]}
    with out.open("a") as f:
        if new:
            f.write("\t".join(FIELDS) + "\n")
        for line in pathlib.Path(a.cases).read_text().splitlines():
            if not line.strip() or line.startswith("#"):
                continue
            parts = line.split()
            case_dir = pathlib.Path(parts[0]) if parts[0].startswith("/") else pathlib.Path(a.repo).resolve() / parts[0]
            if not (case_dir / "nf_input.yaml").exists():   # a case that only the branch tree carries
                alt = pathlib.Path(a.branch).resolve().parent.parent / parts[0]
                if (alt / "nf_input.yaml").exists():
                    case_dir = alt
                else:
                    print(f"{parts[0]}: no nf_input.yaml, skipped"); continue
            base_over = dict(p.split("=", 1) for p in parts[1:])
            text = (case_dir / "nf_input.yaml").read_text()
            slice_sampling = bool(re.search(r"method:\s*SLICE_SAMPLING", text))
            for lever in a.levers.split(","):
                if (parts[0], lever) in done:
                    continue
                binary, env_extra, over = LEVERS[lever]
                if "param1" in over and not slice_sampling:
                    continue
                row = run_one(case_dir, binaries[binary], env_extra, {**base_over, **over}, a.threads, a.timeout)
                if row is None:
                    print(f"{parts[0]} {lever}: override key missing, skipped"); continue
                row.update({"case": parts[0], "lever": lever})
                f.write("\t".join(str(row[k]) for k in FIELDS) + "\n"); f.flush()
                print(f"{parts[0]:60s} {lever:13s} exit={row['exit']} wall={row['wall_s']}s logZ={row['logZ']} calls={row['calls']} steps={row['steps']}  {row['gpu'][:90]}", flush=True)


if __name__ == "__main__":
    main()
