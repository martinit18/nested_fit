// f2c runtime for the device: what f2c's output expects from "f2c.h" and libf2c, as device code,
// plus the legacy Fortran library entry points (WOFZ, VOIGT_IF, GAUSS_IF, LORE_IF, WofzRe) in the
// Fortran calling convention, mapped onto the nested_fit device library. NF_DEV is the device
// qualifier (empty on the host).
#ifndef NF_F2C_SHIM_H
#define NF_F2C_SHIM_H
typedef double doublereal;
typedef float real;
typedef int integer;
typedef int logical;
typedef long int longint;
typedef short int shortint;
typedef unsigned long int uinteger;
typedef char* address;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
#define TRUE_ (1)
#define FALSE_ (0)
#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (doublereal)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (doublereal)min(a,b)
#define dmax(a,b) (doublereal)max(a,b)
NF_DEV double pow_dd(doublereal* a, doublereal* b) { return pow(*a, *b); }
NF_DEV double pow_di(doublereal* a, integer* n) { return nf_powi(*a, *n); }
NF_DEV double pow_ri(real* a, integer* n) { return nf_powi((double)*a, *n); }
NF_DEV integer pow_ii(integer* a, integer* n) { integer r = 1; integer x = *a; integer k = *n; if (k < 0) return (x == 1) ? 1 : (x == -1 ? ((k & 1) ? -1 : 1) : 0); for (; k > 0; --k) r *= x; return r; }
NF_DEV double d_sign(doublereal* a, doublereal* b) { double x = *a >= 0 ? *a : -*a; return *b >= 0 ? x : -x; }
NF_DEV double d_int(doublereal* x) { return *x > 0 ? floor(*x) : -floor(-*x); }
NF_DEV double d_nint(doublereal* x) { return *x >= 0 ? floor(*x + 0.5) : -floor(0.5 - *x); }
NF_DEV integer i_nint(real* x) { return (integer)(*x >= 0 ? floor(*x + 0.5) : -floor(0.5 - *x)); }
NF_DEV integer i_dnnt(doublereal* x) { return (integer)(*x >= 0 ? floor(*x + 0.5) : -floor(0.5 - *x)); }
NF_DEV double d_mod(doublereal* x, doublereal* y) { return fmod(*x, *y); }
NF_DEV double d_lg10(doublereal* x) { return log10(*x); }
NF_DEV double d_exp(doublereal* x) { return exp(*x); }
NF_DEV double d_log(doublereal* x) { return log(*x); }
NF_DEV double d_sqrt(doublereal* x) { return sqrt(*x); }
NF_DEV double derf_(doublereal* x) { return erf(*x); }
NF_DEV double derfc_(doublereal* x) { return erfc(*x); }
NF_DEV double gamma_(doublereal* x) { return tgamma(*x); }
NF_DEV double erf_(real* x) { return erf((double)*x); }
NF_DEV double erfc_(real* x) { return erfc((double)*x); }
// The legacy library, Fortran convention (by reference, trailing underscore), on the device library
NF_DEV int wofz_(doublereal* xi, doublereal* yi, doublereal* u, doublereal* v, logical* flag) {
  // the asymptotic series of the device library's nf_wofz_re, for both parts: w(z) = (i/sqrt(pi) z) sum_k (2k-1)!!/(2z^2)^k
  const double zr = *xi, zi = *yi, dd = zr*zr + zi*zi;
  if (zi > 0.0 && dd > 100.0) {
    const double hr = 0.5*(zr*zr - zi*zi)/(dd*dd), hi = -zr*zi/(dd*dd);
    double tr = 1.0, ti = 0.0, sr = 1.0, si = 0.0;
    for (int k = 1; k < 12; ++k) { const double c = 2*k - 1, nr = c*(tr*hr - ti*hi), ni = c*(tr*hi + ti*hr); tr = nr; ti = ni; sr += tr; si += ti; }
    *u = (zi*sr - zr*si)/(1.7724538509055159*dd); *v = (zr*sr + zi*si)/(1.7724538509055159*dd); *flag = 0; return 0;
  }
  *flag = nf_wofz(zr, zi, u, v) != 0; return 0;
}
NF_DEV double voigt_if_(doublereal* x, doublereal* x0, doublereal* amp, doublereal* sigma, doublereal* gamma) { return VOIGT_IF(*x, *x0, *amp, *sigma, *gamma); }
NF_DEV double gauss_if_(doublereal* x, doublereal* x0, doublereal* amp, doublereal* sigma) { return GAUSS_IF(*x, *x0, *amp, *sigma); }
NF_DEV double lore_if_(doublereal* x, doublereal* x0, doublereal* amp, doublereal* gamma) { return LORE_IF(*x, *x0, *amp, *gamma); }
NF_DEV double wofzre_(doublereal* zr, doublereal* zi) { return WofzRe(*zr, *zi); }
#endif
