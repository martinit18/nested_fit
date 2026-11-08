// nested_fit GPU likelihood backend - C API, called from Fortran via ISO_C_BINDING.
// Driver API + NVRTC, both loaded with dlopen at run time: the build needs neither a
// CUDA toolkit nor a GPU, and on a machine without a driver every call is a no-op.
#pragma once
#ifdef __cplusplus
extern "C" {
#endif

// Preference from the input file: "auto", "cuda", "hip", "cpu". The NESTED_FIT_DEVICE
// environment variable takes precedence. Call before nf_gpu_available().
void nf_gpu_set_preference(const char* device);
void nf_gpu_set_precision(const char* precision);   /* "double" (default) or "mixed": single precision where the model is small against the noise, double elsewhere */
// Mixed precision: the threshold, in units of the data point's noise, above which a term
// (NF_TERM-marked model) or the model value (unmarked model) is recomputed in double. Read at
// every launch, so the sampler can relax it while the live points are far apart and tighten
// it as they converge. 0 = all in double, 1e30 = all in single.
void nf_gpu_set_tolerance(double threshold);
// 1 if a vendor runtime loads and at least one device answers; 0 otherwise.
int nf_gpu_available(void);
// Human-readable device name, or "" when unavailable.
const char* nf_gpu_device_name(void);
// Upload the data once. like_kind: 0 Poisson counts, 1 Gaussian, 2 modified Jeffreys.
// nthreads: OpenMP threads that may call nf_gpu_loglike concurrently (one stream each).
int nf_gpu_init(const double* x, const double* nc, const double* nc_err, int ndata,
                int like_kind, int nthreads);
// Compile the user function. c_body is the expression in C syntax over `x` and the
// parameter identifiers; param_ids is "x1,s1,a1,...". Returns 0 on success.
int nf_gpu_set_function(const char* c_body, const char* param_ids, int npar);
// Log-likelihood WITHOUT const_ll, for one parameter vector, on this thread's stream.
double nf_gpu_loglike(const double* par, int npar, int thread_id);
// The same at a given mixed-precision threshold instead of the current one: measures the
// error ladder at a point of the run without disturbing the other threads' launches.
double nf_gpu_loglike_at(const double* par, int npar, int thread_id, double threshold);
// Last error text (compile log on NVRTC failure), or "".
const char* nf_gpu_last_error(void);
void nf_gpu_free(void);

#ifdef __cplusplus
}
#endif
