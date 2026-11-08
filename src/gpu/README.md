# GPU likelihood backend

`pip install nested_fit`, run as usual: if the machine has a supported GPU, the
likelihood is evaluated on it; if not, nothing changes. One log line says which.

## The contract, per platform

| platform | NVIDIA | AMD | none |
|---|---|---|---|
| Linux | CUDA driver + NVRTC — **verified** | ROCm + hipRTC — written, **unverified** (no AMD hardware reachable) | CPU, unchanged |
| Windows | `nvcuda.dll` + `nvrtc64_*.dll` — written, **unverified** | not supported (CPU) | CPU, unchanged |
| macOS | no NVIDIA support exists on macOS | no ROCm on macOS | CPU, unchanged — always |

Whatever the platform, the install never breaks: the backend is a plain C++17 file
linked statically, with no vendor headers or toolkit at build time, and the vendor
libraries are looked up at run time. The only hard requirement to *use* a GPU is the
driver plus the vendor's runtime compiler (`libnvrtc` — `pip install nested_fit[cuda]`
provides it — or `libhiprtc` from ROCm).

## How

* `nf_gpu.cpp` — loads `libcuda`/`libnvrtc` (or `libamdhip64`/`libhiprtc`) and binds the
  dozen entry points it uses, declared here with their stable C ABI. CUDA is tried
  first, then HIP; `NESTED_FIT_DEVICE` picks explicitly.
* The user's LaTeX expression is already compiled to Fortran at run time; `latex_parser.cpp`
  now also emits it in C (`infixcode_c`: `**` → `pow`, intrinsics → libm). The runtime
  compiler builds that plus one reduction kernel for the exact device, once per model
  per machine (cached in `~/.nested_fit/gpu/`, `%USERPROFILE%` on Windows).
* `Mod_gpu.f90` binds the C API. `Mod_likelihood.f90` hands the data over after
  `INIT_LIKELIHOOD_FUNC` (`GPU_SETUP_DATA`); when `gpu_on`, `LOGLIKELIHOOD_1D` returns
  the device value plus `const_ll`. One stream per OpenMP thread.

Same closed forms as the Fortran, double precision, no fast-math: the GPU changes the
time, never the answer. A device fault stops the run rather than returning a value.

## Scope today

1D data, one set, Poisson / Gaussian / modified-Jeffreys likelihoods, user functions
built from the arithmetic intrinsics plus `GAUSS_IF`, `LORE_IF`, `VOIGT_IF` and `WofzRe`
(the Faddeeva function is ported line by line from `WOFZ.f`, single-precision literals
included, so the device returns the CPU's bits; in the wings, |z| > 10, the asymptotic series
replaces it: exact to 3 × 10⁻¹⁵, 1.2× in the sampler on a 284-term Voigt model,
`NF_NO_WING_SERIES` disables it). Anything else (`Interpolate`, nested user
functions, sets, 2D) runs on the CPU as before, and the log says why.

**Legacy Fortran user functions** (`USERFCN.f`) run on the device when the input names a C
file that defines the same function:

```yaml
function:
  expression: TM_STEP_ANCHOR_FULL2
  gpu_source: tm_step_anchor_full2.c    # defines nf_user(x, params); params[i] is npar = i+1
```

The file is plain C with the device library in scope (`VOIGT_IF`, `erf`, `exp`, ...), compiled
for the GPU at hand like an expression is; the CPU keeps calling the Fortran. Writing the C
is the same work as writing the Fortran was. The Fortran itself can come from a file beside the
input (`function.source`, see the main README): then neither side needs a rebuild of nested_fit.
And `function.source` can be the C file itself: the CPU compiles it with g++ behind a wrapper
with the legacy calling convention, the GPU with NVRTC, no `gpu_source` and no Fortran at all.

**Precision.** `search.gpu_precision` (env `NESTED_FIT_GPU_PRECISION`):

* `auto` (default): `mixed` when the model marks its terms with `NF_TERM`, `double` otherwise.
* `double`: the CPU's closed forms, the CPU's bits.
* `mixed`: per-term precision. A model written with `NF_TERM(multiplier, function, args...)`
  around its additive terms has each term evaluated in single precision and recomputed in
  double only when it exceeds a tenth of the data's noise at that point
  (`NESTED_FIT_GPU_TERM_TOL`); on the CPU and in double the macro is the plain product.
  Measured on a 284-term Voigt model: 1.6 × 10⁻⁸ relative to the CPU, 9.5× the GPU's double
  rate on an RTX A4000 (789 against 83 evaluations per second in the sampler, 954 with the
  wing series of the Faddeeva function). The model in float throughout was measured too and
  rejected: on the same model the float error of the likelihood is 10⁻⁴ relative, point
  dependent, and far more than the evidence tolerates.

**Threads.** `search.gpu_threads` (env `NESTED_FIT_GPU_THREADS`): how many of the OpenMP
threads evaluate on the GPU; the others keep the CPU closed forms. -1 (default) leaves the choice
to the startup self-test, from r = one evaluation on one CPU thread over one on the GPU: when
4 r < threads the GPU is not used, otherwise the run keeps at most 8 threads, all of them on the
GPU. Measured on an EPYC 7452 (128 threads) with an RTX A4000, per-term precision, 100 steps of
the Voigt model: 8 threads on the GPU 26 s; 128 threads on the GPU 60 s (more in-flight
evaluations, 93 ms per launch); 8 on the GPU and 120 on the CPU over 900 s (the CPU threads starve
the ones feeding the GPU). An explicit number is kept as given, that many on the GPU and the rest
on the CPU: `gpu_threads: 2` with the other threads on the cores was 1.47× the GPU alone in double
precision on an 8-thread host, and is a setting, not a default.

**Startup self-test.** When a device is used, the likelihood is evaluated on both backends at
three points of the prior box before sampling starts. The log reports the largest relative
difference and the time of one evaluation on the device against one CPU thread; above 10⁻⁸
in double, or 10⁻⁶ in per-term mixed precision, the device is refused and the run continues
on the CPU. A wrong port of a user function cannot reach the evidence. With `device: auto` the
GPU is also refused when, fed by 8 threads, it would not beat the CPU threads together, i.e. when
4 r < threads (a cheap model on a few thousand points: every example of the suite); otherwise the
run keeps at most 8 threads, all on the GPU (see Threads); `device: cuda` forces the GPU.

One parameter vector per launch: about break-even with a CPU thread at ~1000 data points of a
cheap model, growing with data size and with the cost per point. Batching the search's
candidates is the next step.

## Controls

In the input file, beside the other search parameters — so a run is reproducible from
its `nf_input.yaml` alone, and `nf_output_res.json` records `input.search.device`:

```yaml
search:
  device:         auto     # auto (default) | cuda | hip | cpu
  gpu_min_points: 0        # use the GPU only above this many data points
  gpu_threads:    -1       # -1 (default): the self-test keeps at most 8 threads, all on the GPU; N: N on the GPU, the others on the CPU
  gpu_precision:  auto     # auto (default: mixed if the model uses NF_TERM) | double | mixed
```

Environment variables override the file (useful for sweeps without editing it):

* `NESTED_FIT_DEVICE` — `auto`, `cuda`, `hip`, `cpu`.
* `function.gpu_source` (input file only) — device source of a legacy user function, see above.
* `NESTED_FIT_GPU_INDEX=n` — device index (default 0).
* `NESTED_FIT_GPU_MIN_POINTS=n` — as above.
* `NESTED_FIT_GPU_THREADS=n`, `NESTED_FIT_GPU_PRECISION=double|mixed`, `NESTED_FIT_GPU_TERM_TOL=0.1` — as above.
* `NESTED_FIT_GPU_ARCH=gfx90a` — HIP only: target architecture, if hipRTC's default is wrong.
* `NESTED_FIT_NVRTC_PATH`, `NESTED_FIT_CUDA_PATH`, `NESTED_FIT_HIP_PATH`, `NESTED_FIT_HIPRTC_PATH` —
  full path to a library when it is not on the loader's search path (pynested_fit sets
  the first automatically when the `nvidia-cuda-nvrtc` wheel is installed).

## Checks — Gate 0, measured 2026-10-23 (RTX 4090, driver 610.43, 48 threads)

| case | CPU log Z | GPU log Z | \|z\| | max lnL CPU / GPU | calls CPU / GPU | wall CPU → GPU |
|---|---|---|---|---|---|---|
| `test_12` Gaussian, 1000 pts | −951.388 ± 0.120 | −951.317 ± 0.120 | 0.42 | −918.94137 / −918.94196 | 14.95 M / 14.94 M | 43 s → 298 s |
| `test_01` Poisson, 1024 pts | −574.823 ± 0.030 | −574.835 ± 0.030 | 0.29 | −564.06237 / −564.06236 | 25.27 M / 25.30 M | 89 s → 599 s |
| `test_12`, multi-block kernel, Tesla V100, 26 Oct. | −951.388 ± 0.120 | −951.365 ± 0.120 | 0.14 | −918.94137 / −918.94166 | 14.95 M / 14.82 M | 101 s on the V100 box |
| `test_12`, this backend (double + per-term + thread split), RTX A4000, 5 Nov. | −951.19 to −951.28 ± 0.12 (dev 5.7.1, 4 runs) | −951.308 ± 0.120 | < 1 | −918.9414 / −918.9407 | 14.74 M / 14.78 M | 64 s on the A4000 box |

Measured on a realistic model (Gonçalo's Tm spectrum, 16 501 points, ~284 Faddeeva per
point, 0.3 s per call on one core): V100 1 100 evaluations per second in double precision,
RTX A4000 83 with 8 host threads and 108 with 2; per-term mixed precision on the A4000 789
(4 Nov.) and 1 350 (5 Nov., 24-core host) per second, self-test 1.6 × 10⁻⁸ — details in
`REPORT_2026-11-05_proposal.md`.

**Correctness: passed.** Same evidence within the combined uncertainty, same maximum
likelihood, same number of likelihood calls — the GPU changed only the time.

**Time, 23 Oct. kernel: as the model predicts for one evaluation per launch — about 7× slower than the CPU
at ~1000 points.** ~20 µs per launch, and the host threads serialise on the driver so
launches do not overlap. This is the measured reason the batched slice sampling of the
design note is the next step rather than an option; the backend is correct, the launch
structure is not yet efficient.

On a machine without a driver the log reads `GPU: not used (...)` and the result is
unchanged (verified on maqi-research). Windows and ROCm need one run each by someone who
has them.
