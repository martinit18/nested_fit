// nested_fit GPU likelihood backend. See nf_gpu.h and README.md.
//
// Vendor-neutral loader: the CUDA driver API + NVRTC, or HIP + hipRTC, resolved at run
// time from the vendor's shared library through a dozen entry points declared here
// with their stable C ABI. No vendor headers or toolkit at build time, so this file
// builds with any C++17 compiler on Linux, macOS and Windows; where no driver is
// present every call is a no-op and the Fortran side stays on the CPU.
#include "nf_gpu.h"
#include "nf_device_lib_src.h"
#include <algorithm>
#include <atomic>
#include <regex>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <sstream>
#include <string>
#include <vector>

#if defined(_WIN32)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
static void* lib_open(const char* n) { return reinterpret_cast<void*>(LoadLibraryA(n)); }
static void* lib_sym(void* h, const char* n) { return h ? reinterpret_cast<void*>(GetProcAddress(reinterpret_cast<HMODULE>(h), n)) : nullptr; }
#else
#  include <dlfcn.h>
static void* lib_open(const char* n) { return dlopen(n, RTLD_NOW | RTLD_GLOBAL); }
static void* lib_sym(void* h, const char* n) { return h ? dlsym(h, n) : nullptr; }
#endif

namespace {

using devptr = unsigned long long;   // CUdeviceptr / hipDeviceptr_t: both 64-bit
enum class Vendor { none, cuda, hip };

// One table for both vendors. Where a HIP call has a different name but the same
// shape it is listed beside the CUDA one; the two genuinely different ones
// (context selection, error strings) are handled explicitly.
struct Api {
    int (*init)(unsigned);
    int (*deviceGetCount)(int*);
    int (*deviceGet)(int*, int);
    int (*deviceGetName)(char*, int, int);
    int (*deviceGetAttribute)(int*, int, int);          // CUDA only (compute capability)
    int (*primaryCtxRetain)(void**, int);                // CUDA only
    int (*ctxSetCurrent)(void*);                         // CUDA only
    int (*setDevice)(int);                               // HIP only
    int (*moduleLoadData)(void**, const void*);
    int (*moduleGetFunction)(void**, void*, const char*);
    int (*memAlloc)(devptr*, size_t);
    int (*memFree)(devptr);
    int (*memcpyHtoD)(devptr, const void*, size_t);
    int (*memcpyHtoDAsync)(devptr, const void*, size_t, void*);
    int (*memcpyDtoHAsync)(void*, devptr, size_t, void*);
    int (*streamCreate)(void**, unsigned);
    int (*streamSynchronize)(void*);
    int (*launchKernel)(void*, unsigned, unsigned, unsigned, unsigned, unsigned, unsigned, unsigned, void*, void**, void**);
    int (*cuErrorString)(int, const char**);             // CUDA
    const char* (*hipErrorString)(int);                  // HIP
    int (*rtcCreate)(void**, const char*, const char*, int, const char* const*, const char* const*);
    int (*rtcCompile)(void*, int, const char* const*);
    int (*rtcGetCodeSize)(void*, size_t*);
    int (*rtcGetCode)(void*, char*);
    int (*rtcGetLogSize)(void*, size_t*);
    int (*rtcGetLog)(void*, char*);
    int (*rtcDestroy)(void**);
    const char* (*rtcErrorString)(int);
};

struct State {
    bool probed = false, available = false, ready = false;
    Vendor vendor = Vendor::none;
    Api a{};
    int dev = 0, cc_major = 0, cc_minor = 0, ndata = 0, like_kind = 0, nthreads = 1, npar = 0;
    void* ctx = nullptr; void* mod = nullptr; void* kern = nullptr;
    devptr dx = 0, dnc = 0, derr = 0;
    std::vector<void*> streams; std::vector<devptr> dpar, dout, dpart, dcount; int nblocks = 1;
    bool mixed = false;   // single precision where the model is small against the noise: per term (NF_TERM) or per point
    bool per_point = false;   // mixed without NF_TERM markers: the whole model in float, recomputed in double where large
    std::atomic<float> tol{0.1f};   // the threshold of the mixed rule, in noise units, read at every launch
    std::string name, err;
} S;

const char* env(const char* k) { const char* v = std::getenv(k); return (v && *v) ? v : nullptr; }

void* open_any(std::initializer_list<const char*> names, const char* hint_env) {
    if (const char* p = env(hint_env)) if (void* h = lib_open(p)) return h;
    for (const char* n : names) if (void* h = lib_open(n)) return h;
    return nullptr;
}

template <typename F> bool bind(F& f, void* h, const char* name) {
    f = reinterpret_cast<F>(lib_sym(h, name));
    if (!f) S.err = std::string("missing symbol ") + name;
    return f != nullptr;
}

bool load_cuda() {
#if defined(__APPLE__)
    S.err = "no NVIDIA driver on macOS (CPU only)"; return false;
#elif defined(_WIN32)
    void* cu = open_any({ "nvcuda.dll" }, "NESTED_FIT_CUDA_PATH");
    void* rt = open_any({ "nvrtc64_130_0.dll", "nvrtc64_120_0.dll" }, "NESTED_FIT_NVRTC_PATH");
#else
    void* cu = open_any({ "libcuda.so.1", "libcuda.so" }, "NESTED_FIT_CUDA_PATH");
    void* rt = open_any({ "libnvrtc.so.13", "libnvrtc.so.12", "libnvrtc.so" }, "NESTED_FIT_NVRTC_PATH");
#endif
#if !defined(__APPLE__)
    if (!cu) { S.err = "no NVIDIA driver (libcuda / nvcuda.dll not found)"; return false; }
    if (!rt) { S.err = "NVRTC not found: pip install nvidia-cuda-nvrtc-cu12, or install the CUDA toolkit"; return false; }
    Api& a = S.a;
    // the _v2 names are the current 64-bit entry points; the unsuffixed ones are legacy
    return bind(a.init, cu, "cuInit") && bind(a.deviceGetCount, cu, "cuDeviceGetCount") && bind(a.deviceGet, cu, "cuDeviceGet")
        && bind(a.deviceGetName, cu, "cuDeviceGetName") && bind(a.deviceGetAttribute, cu, "cuDeviceGetAttribute")
        && bind(a.primaryCtxRetain, cu, "cuDevicePrimaryCtxRetain") && bind(a.ctxSetCurrent, cu, "cuCtxSetCurrent")
        && bind(a.moduleLoadData, cu, "cuModuleLoadData") && bind(a.moduleGetFunction, cu, "cuModuleGetFunction")
        && bind(a.memAlloc, cu, "cuMemAlloc_v2") && bind(a.memFree, cu, "cuMemFree_v2") && bind(a.memcpyHtoD, cu, "cuMemcpyHtoD_v2")
        && bind(a.memcpyHtoDAsync, cu, "cuMemcpyHtoDAsync_v2") && bind(a.memcpyDtoHAsync, cu, "cuMemcpyDtoHAsync_v2")
        && bind(a.streamCreate, cu, "cuStreamCreate") && bind(a.streamSynchronize, cu, "cuStreamSynchronize")
        && bind(a.launchKernel, cu, "cuLaunchKernel") && bind(a.cuErrorString, cu, "cuGetErrorString")
        && bind(a.rtcCreate, rt, "nvrtcCreateProgram") && bind(a.rtcCompile, rt, "nvrtcCompileProgram")
        && bind(a.rtcGetCodeSize, rt, "nvrtcGetPTXSize") && bind(a.rtcGetCode, rt, "nvrtcGetPTX")
        && bind(a.rtcGetLogSize, rt, "nvrtcGetProgramLogSize") && bind(a.rtcGetLog, rt, "nvrtcGetProgramLog")
        && bind(a.rtcDestroy, rt, "nvrtcDestroyProgram") && bind(a.rtcErrorString, rt, "nvrtcGetErrorString");
#endif
}

bool load_hip() {
#if defined(__APPLE__) || defined(_WIN32)
    S.err = "no ROCm on this platform"; return false;
#else
    void* hp = open_any({ "libamdhip64.so.7", "libamdhip64.so.6", "libamdhip64.so.5", "libamdhip64.so" }, "NESTED_FIT_HIP_PATH");
    void* rt = open_any({ "libhiprtc.so.7", "libhiprtc.so.6", "libhiprtc.so.5", "libhiprtc.so" }, "NESTED_FIT_HIPRTC_PATH");
    if (!hp) { S.err = "no ROCm runtime (libamdhip64 not found)"; return false; }
    if (!rt) { S.err = "hipRTC not found (libhiprtc)"; return false; }
    Api& a = S.a;
    return bind(a.init, hp, "hipInit") && bind(a.deviceGetCount, hp, "hipGetDeviceCount") && bind(a.deviceGet, hp, "hipDeviceGet")
        && bind(a.deviceGetName, hp, "hipDeviceGetName") && bind(a.setDevice, hp, "hipSetDevice")
        && bind(a.moduleLoadData, hp, "hipModuleLoadData") && bind(a.moduleGetFunction, hp, "hipModuleGetFunction")
        && bind(a.memAlloc, hp, "hipMalloc") && bind(a.memFree, hp, "hipFree") && bind(a.memcpyHtoD, hp, "hipMemcpyHtoD")
        && bind(a.memcpyHtoDAsync, hp, "hipMemcpyHtoDAsync") && bind(a.memcpyDtoHAsync, hp, "hipMemcpyDtoHAsync")
        && bind(a.streamCreate, hp, "hipStreamCreateWithFlags") && bind(a.streamSynchronize, hp, "hipStreamSynchronize")
        && bind(a.launchKernel, hp, "hipModuleLaunchKernel") && bind(a.hipErrorString, hp, "hipGetErrorString")
        && bind(a.rtcCreate, rt, "hiprtcCreateProgram") && bind(a.rtcCompile, rt, "hiprtcCompileProgram")
        && bind(a.rtcGetCodeSize, rt, "hiprtcGetCodeSize") && bind(a.rtcGetCode, rt, "hiprtcGetCode")
        && bind(a.rtcGetLogSize, rt, "hiprtcGetProgramLogSize") && bind(a.rtcGetLog, rt, "hiprtcGetProgramLog")
        && bind(a.rtcDestroy, rt, "hiprtcDestroyProgram") && bind(a.rtcErrorString, rt, "hiprtcGetErrorString");
#endif
}

std::string errstr(int r) {
    if (S.a.cuErrorString) { const char* s = nullptr; S.a.cuErrorString(r, &s); return s ? s : "CUDA error"; }
    if (S.a.hipErrorString) { const char* s = S.a.hipErrorString(r); return s ? s : "HIP error"; }
    return "device error " + std::to_string(r);
}
bool ok(int r, const char* what) { if (r == 0) return true; S.err = std::string(what) + ": " + errstr(r); return false; }

std::string g_pref = "auto";   // from the input file; the environment wins

void probe() {
    S.probed = true;
    std::string want = env("NESTED_FIT_DEVICE") ? env("NESTED_FIT_DEVICE") : g_pref;
    if (want == "cpu") { S.err = "NESTED_FIT_DEVICE=cpu"; return; }
    bool loaded = false;
    if (want == "auto" || want == "cuda" || want == "gpu") { loaded = load_cuda(); if (loaded) S.vendor = Vendor::cuda; }
    if (!loaded && (want == "auto" || want == "hip" || want == "rocm" || want == "gpu")) {
        std::string cuda_err = S.err; S.a = Api{};
        loaded = load_hip(); if (loaded) S.vendor = Vendor::hip; else S.err = cuda_err + "; " + S.err;
    }
    if (!loaded) return;
    int n = 0;
    if (!ok(S.a.init(0), "init") || !ok(S.a.deviceGetCount(&n), "deviceGetCount")) return;
    if (n < 1) { S.err = "no GPU device found"; return; }
    int idx = env("NESTED_FIT_GPU_INDEX") ? std::atoi(env("NESTED_FIT_GPU_INDEX")) : 0;
    char buf[256] = { 0 };
    if (!ok(S.a.deviceGet(&S.dev, idx), "deviceGet") || !ok(S.a.deviceGetName(buf, sizeof buf, S.dev), "deviceGetName")) return;
    if (S.vendor == Vendor::cuda) {
        S.a.deviceGetAttribute(&S.cc_major, 75, S.dev);   // CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR
        S.a.deviceGetAttribute(&S.cc_minor, 76, S.dev);   // ..._MINOR
        if (!ok(S.a.primaryCtxRetain(&S.ctx, S.dev), "primaryCtxRetain") || !ok(S.a.ctxSetCurrent(S.ctx), "ctxSetCurrent")) return;
    } else if (!ok(S.a.setDevice(S.dev), "setDevice")) return;
    S.name = buf; S.available = true;
}

void make_current() { if (S.vendor == Vendor::cuda) S.a.ctxSetCurrent(S.ctx); else if (S.a.setDevice) S.a.setDevice(S.dev); }

// The device program: the device library, the user's function and one reduction kernel. One
// block of 256 threads per 256 data points; each block reduces its share in shared memory and
// the last block to finish adds the block partials in a fixed order. Double precision, no
// fast-math: the same closed forms as LOGLIKELIHOOD_1D, the same bits run after run. The
// source is plain CUDA C that hipRTC also accepts.
// Mixed precision needs a float copy of the model: the library and the user's function
// compiled once more with `double` meaning `float` and every literal made a float, so no
// operation of that copy falls back to fp64; the residuals and the sum stay in double.
// With NF_TERM markers the choice is made per term; without, per data point: the whole
// model in float, and again in double where its value is large against the noise there.
std::string single_literals(std::string s) {
    // std::regex has no lookbehind: the character before the literal is matched and kept
    static const std::regex lit(R"((^|[^\w.])(\d+\.\d*(?:[eE][-+]?\d+)?|\.\d+(?:[eE][-+]?\d+)?|\d+[eE][-+]?\d+)(?![fF\d.]))");   // 1.0, 1., .5, 1e-3
    return std::regex_replace(s, lit, "$1$2f");
}

std::string program(const std::string& body, const std::string& ids, bool mixed, int npar) {
    std::ostringstream p, u;
    S.per_point = mixed && body.find("NF_TERM") == std::string::npos;
    if (S.vendor == Vendor::hip) p << "#include <hip/hip_runtime.h>\n";
    if (body.find("nf_user") != std::string::npos) {
        u << body << "\n";   // a complete device source (function.gpu_source): it defines nf_user itself
    } else {
        u << "__device__ __forceinline__ double nf_user(double x, const double* params){\n"
             "  const double pi = NF_PI; (void)pi;\n";
        std::stringstream ss(ids); std::string id; int i = 0;
        while (std::getline(ss, id, ',')) if (!id.empty()) u << "  const double " << id << " = params[" << i++ << "];\n";
        u << "  return " << body << ";\n}\n";
    }
    if (mixed) {
        // the model twice, in float (nf32) and in double (nf64). Per term: in nf64 an NF_TERM is
        // evaluated in float first and in double only when it is large against the noise scale
        // of the data point (nf_scale, set per thread by the kernel) times the threshold nf_tol,
        // a launch argument. Per point: the kernel applies the same test to the whole model.
        p << "__shared__ float nf_scale[256]; __shared__ float nf_tol;\n"
             "namespace nf32 {\n#define NF_TERM(mult, f, ...) ((mult)*f(__VA_ARGS__))\n#define NF_SINGLE 1\n#define double float\n"
          << single_literals(NF_DEVICE_LIB) << "\n" << single_literals(u.str())
          << "#undef double\n#undef NF_SINGLE\n#undef NF_TERM\n}\n"
             "template <class G> __device__ __forceinline__ double nf_term_select(double mult, float v32, G g64){\n"
             "  if (fabsf((float)mult*v32) > nf_tol*nf_scale[threadIdx.x]) return mult*g64();\n"
             "  return mult*(double)v32; }\n"
             "namespace nf64 {\n#define NF_TERM(mult, f, ...) nf_term_select((mult), nf32::f(__VA_ARGS__), [&]{ return f(__VA_ARGS__); })\n"
          << NF_DEVICE_LIB << "\n" << u.str() << "#undef NF_TERM\n}\n";
        if (S.per_point) p << "__shared__ float nf_pf[" << (npar > 0 ? npar : 1) << "];\n";
    }
    else p << NF_DEVICE_LIB << "\n" << u.str();
    p << "extern \"C\" __global__ void nf_loglike(const double* __restrict__ x, const double* __restrict__ nc,"
         " const double* __restrict__ err, int n, int kind, const double* __restrict__ params, float tol,"
         " double* partial, unsigned int* count, double* out){\n"
         "  __shared__ double red[256]; __shared__ int last; double acc = 0.0;\n";
    if (mixed) {
        p << "  if (threadIdx.x == 0) nf_tol = tol;\n";
        if (S.per_point) p << "  for (int k = threadIdx.x; k < " << (npar > 0 ? npar : 1) << "; k += blockDim.x) nf_pf[k] = (float)params[k];\n";
        p << "  __syncthreads();\n"
             "  for (int i = blockIdx.x*blockDim.x + threadIdx.x; i < n; i += gridDim.x*blockDim.x){\n"
             "    nf_scale[threadIdx.x] = (kind == 0) ? sqrtf(fmaxf((float)nc[i], 1.0f)) : (float)err[i];\n";
        if (S.per_point)
            p << "    float m32 = nf32::nf_user((float)x[i], nf_pf);\n"
                 "    double m = (fabsf(m32) > nf_tol*nf_scale[threadIdx.x]) ? nf64::nf_user(x[i], params) : (double)m32;\n";
        else
            p << "    double m = nf64::nf_user(x[i], params);\n";
    }
    else
        p << "  for (int i = blockIdx.x*blockDim.x + threadIdx.x; i < n; i += gridDim.x*blockDim.x){ double m = nf_user(x[i], params);\n";
    p <<
         "    if (kind == 0) acc += nc[i]*log(m) - m;\n"
         "    else if (kind == 1){ double d = nc[i]-m; acc -= d*d/(2.0*err[i]*err[i]); }\n"
         "    else { double rk = (nc[i]-m)/err[i]; double rk2 = rk*rk; acc += log((1.0-exp(-rk2/2.0))/rk2); } }\n"
         "  red[threadIdx.x] = acc; __syncthreads();\n"
         "  for (int s = blockDim.x/2; s > 0; s >>= 1){ if (threadIdx.x < s) red[threadIdx.x] += red[threadIdx.x+s]; __syncthreads(); }\n"
         // the block's partial sum; the last block to finish adds the partials in a fixed order,
         // so the result does not depend on the scheduling (same bits run after run)
         "  if (threadIdx.x == 0){ partial[blockIdx.x] = red[0]; __threadfence(); last = (atomicAdd(count, 1u) == gridDim.x - 1); }\n"
         "  __syncthreads();\n"
         "  if (last && threadIdx.x == 0){ volatile double* vp = partial; double s = 0.0;\n"
         "    for (unsigned b = 0; b < gridDim.x; ++b) s += vp[b];\n"
         "    *out = s; *count = 0u; }\n}\n";
    return p.str();
}

std::filesystem::path cache_dir() {
    const char* h = env("HOME"); if (!h) h = env("USERPROFILE"); if (!h) h = ".";
    std::filesystem::path d = std::filesystem::path(h) / ".nested_fit" / "gpu";
    std::error_code ec; std::filesystem::create_directories(d, ec);
    return d;
}

bool compile(const std::string& src, void*& mod, void*& kern) {
    // Compiled code cached per (source, device, target): a model compiles once per machine.
    std::string target = S.vendor == Vendor::cuda ? "compute_" + std::to_string(S.cc_major) + std::to_string(S.cc_minor)
                                                  : (env("NESTED_FIT_GPU_ARCH") ? env("NESTED_FIT_GPU_ARCH") : "default");
    std::string key = std::to_string(std::hash<std::string>{}(src + S.name + target));
    std::filesystem::path path = cache_dir() / (key + (S.vendor == Vendor::cuda ? ".ptx" : ".co"));
    std::string code;
    if (std::ifstream f(path, std::ios::binary); f) { std::stringstream b; b << f.rdbuf(); code = b.str(); }
    if (code.empty()) {
        void* prog = nullptr;
        if (S.a.rtcCreate(&prog, src.c_str(), "nf_user.cu", 0, nullptr, nullptr) != 0) { S.err = "rtcCreateProgram failed"; return false; }
        std::vector<std::string> o = { "--std=c++17" };
        if (S.vendor == Vendor::cuda) { o.push_back("--gpu-architecture=" + target); o.push_back("-default-device"); }
        else { o.push_back("-O3"); if (env("NESTED_FIT_GPU_ARCH")) o.push_back("--offload-arch=" + target); }
        std::vector<const char*> opts; for (auto& s : o) opts.push_back(s.c_str());
        int r = S.a.rtcCompile(prog, static_cast<int>(opts.size()), opts.data());
        size_t lsz = 0; S.a.rtcGetLogSize(prog, &lsz); std::string log(lsz, '\0'); if (lsz) S.a.rtcGetLog(prog, log.data());
        if (r != 0) { S.err = std::string("RTC: ") + S.a.rtcErrorString(r) + "\n" + log; S.a.rtcDestroy(&prog); return false; }
        size_t csz = 0; S.a.rtcGetCodeSize(prog, &csz); code.assign(csz, '\0'); S.a.rtcGetCode(prog, code.data()); S.a.rtcDestroy(&prog);
        std::ofstream(path, std::ios::binary) << code;
    }
    return ok(S.a.moduleLoadData(&mod, code.c_str()), "moduleLoadData") && ok(S.a.moduleGetFunction(&kern, mod, "nf_loglike"), "moduleGetFunction");
}

// A device fault mid-run must stop the run, not feed the sampler a wrong value.
void die(int r, const char* what) {
    std::fprintf(stderr, "nested_fit GPU backend: %s failed: %s\n", what, errstr(r).c_str());
    std::exit(3);
}

} // namespace

void nf_gpu_set_preference(const char* device) { if (device && *device && !S.probed) g_pref = device; }
void nf_gpu_set_precision(const char* precision) { const char c = precision ? precision[0] : 'd'; S.mixed = (c == 'm' || c == 'M'); }
void nf_gpu_set_tolerance(double threshold) { S.tol.store(static_cast<float>(threshold)); }
int nf_gpu_available(void) { if (!S.probed) probe(); return S.available ? 1 : 0; }
const char* nf_gpu_device_name(void) { return nf_gpu_available() ? S.name.c_str() : ""; }
const char* nf_gpu_last_error(void) { return S.err.c_str(); }

int nf_gpu_init(const double* x, const double* nc, const double* nc_err, int ndata, int like_kind, int nthreads) {
    if (!nf_gpu_available()) return 1;
    make_current();
    S.ndata = ndata; S.like_kind = like_kind; S.nthreads = nthreads < 1 ? 1 : nthreads;
    size_t bytes = sizeof(double) * ndata;
    if (!ok(S.a.memAlloc(&S.dx, bytes), "memAlloc") || !ok(S.a.memAlloc(&S.dnc, bytes), "memAlloc") || !ok(S.a.memAlloc(&S.derr, bytes), "memAlloc")) return 2;
    if (!ok(S.a.memcpyHtoD(S.dx, x, bytes), "memcpyHtoD") || !ok(S.a.memcpyHtoD(S.dnc, nc, bytes), "memcpyHtoD") || !ok(S.a.memcpyHtoD(S.derr, nc_err, bytes), "memcpyHtoD")) return 2;
    // one block of 256 threads per 256 data points (at most 1024 blocks): a large data set
    // spreads over the whole device, a small one costs one block as before
    S.nblocks = std::min(1024, (ndata + 255)/256); if (S.nblocks < 1) S.nblocks = 1;
    S.streams.assign(S.nthreads, nullptr); S.dpar.assign(S.nthreads, 0); S.dout.assign(S.nthreads, 0);
    S.dpart.assign(S.nthreads, 0); S.dcount.assign(S.nthreads, 0);
    const unsigned int zero = 0u;
    for (int t = 0; t < S.nthreads; ++t)
        if (!ok(S.a.streamCreate(&S.streams[t], 1), "streamCreate") || !ok(S.a.memAlloc(&S.dout[t], sizeof(double)), "memAlloc")
            || !ok(S.a.memAlloc(&S.dpart[t], sizeof(double) * S.nblocks), "memAlloc") || !ok(S.a.memAlloc(&S.dcount[t], sizeof(unsigned int)), "memAlloc")
            || !ok(S.a.memcpyHtoD(S.dcount[t], &zero, sizeof(unsigned int)), "memcpyHtoD")) return 3;
    return 0;
}

int nf_gpu_set_function(const char* c_body, const char* param_ids, int npar) {
    if (!nf_gpu_available() || !c_body || !*c_body) { if (S.err.empty()) S.err = "no C body for the user function"; return 1; }
    make_current();
    S.npar = npar;
    for (int t = 0; t < S.nthreads; ++t) if (!ok(S.a.memAlloc(&S.dpar[t], sizeof(double) * (npar > 0 ? npar : 1)), "memAlloc")) return 2;
    const std::string ids = param_ids ? param_ids : "";
    if (!compile(program(c_body, ids, S.mixed, npar), S.mod, S.kern)) return 3;
    S.ready = true; return 0;
}

// What the device did, printed once at exit: evaluations and the average wall time per launch
// as the host threads saw it (queueing included), so a user reads the gain in the log.
struct LaunchStats {
    std::atomic<unsigned long long> n{0}, ns{0};
    ~LaunchStats() {
        if (n > 0) std::fprintf(stderr, "GPU: %llu likelihood evaluations on the device in %s precision, %.3f ms each as seen by the host threads\n",
                                static_cast<unsigned long long>(n), S.mixed ? (S.per_point ? "per-point mixed" : "per-term mixed") : "double", 1e-6 * ns / n);
    }
} g_stats;

static double launch(const double* par, int npar, int thread_id, float tol) {
    int t = (thread_id < 0 || thread_id >= S.nthreads) ? 0 : thread_id;
    auto t0 = std::chrono::steady_clock::now();
    void* st = S.streams[t]; double out = 0.0; int r;
    make_current();
    if ((r = S.a.memcpyHtoDAsync(S.dpar[t], par, sizeof(double) * npar, st))) die(r, "memcpyHtoDAsync");
    void* args[] = { &S.dx, &S.dnc, &S.derr, &S.ndata, &S.like_kind, &S.dpar[t], &tol, &S.dpart[t], &S.dcount[t], &S.dout[t] };
    if ((r = S.a.launchKernel(S.kern, static_cast<unsigned>(S.nblocks), 1, 1, 256, 1, 1, 0, st, args, nullptr))) die(r, "launchKernel");
    if ((r = S.a.memcpyDtoHAsync(&out, S.dout[t], sizeof(double), st))) die(r, "memcpyDtoHAsync");
    if ((r = S.a.streamSynchronize(st))) die(r, "streamSynchronize");
    g_stats.n += 1;
    g_stats.ns += std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::steady_clock::now() - t0).count();
    return out;
}
double nf_gpu_loglike(const double* par, int npar, int thread_id) { return launch(par, npar, thread_id, S.tol.load()); }
double nf_gpu_loglike_at(const double* par, int npar, int thread_id, double threshold) { return launch(par, npar, thread_id, static_cast<float>(threshold)); }

void nf_gpu_free(void) {
    if (!S.available) return;
    make_current();
    for (auto p : S.dpar) if (p) S.a.memFree(p);
    for (auto p : S.dout) if (p) S.a.memFree(p);
    for (auto p : S.dpart) if (p) S.a.memFree(p);
    for (auto p : S.dcount) if (p) S.a.memFree(p);
    if (S.dx) S.a.memFree(S.dx); if (S.dnc) S.a.memFree(S.dnc); if (S.derr) S.a.memFree(S.derr);
    S.dpar.clear(); S.dout.clear(); S.dpart.clear(); S.dcount.clear(); S.dx = S.dnc = S.derr = 0; S.ready = false;
}
