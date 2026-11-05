// Device-side library of the nested_fit GPU backend: the closed forms the Fortran uses,
// in plain C that NVRTC and hipRTC compile beside the user's function. The same file
// compiles on the host with NF_DEV defined empty (the cross-check harness does that).
#ifndef NF_DEV
#define NF_DEV __device__ __forceinline__
#endif
#define NF_PI 3.141592653589793

NF_DEV double GAUSS_IF(double x, double x0, double amp, double sigma) {
  double e = (x - x0)*(x - x0)/(2.0*sigma*sigma);
  return fabs(e) < 700.0 ? amp/(sqrt(2.0*NF_PI)*sigma)*exp(-e) : 0.0;
}
NF_DEV double LORE_IF(double x, double x0, double amp, double gamma) {
  return amp*gamma/(NF_PI*((x - x0)*(x - x0) + gamma*gamma));
}

// x**n for integer n as libgcc's __powidf2 computes it (what gfortran emits for H2**KAPN).
NF_DEV double nf_powi(double x, int m) {
  unsigned n = m < 0 ? -m : m; double y = (n % 2) ? x : 1.0;
  while (n >>= 1) { x = x*x; if (n % 2) y = y*x; }
  return m < 0 ? 1.0/y : y;
}

// Faddeeva function w(z) = exp(-z^2) erfc(-iz), Poppe & Wijers, ACM TOMS Algorithm 680,
// ported line by line from src/functions/WOFZ.f as nested_fit compiles it: the default-real
// literals of that file (6.3, 4.4, 0.85, 1.88, 1.0/J) are single precision there, and they
// are single precision here, so the device returns the CPU's values. Returns 1 where the
// Fortran sets FLAG (overflow), with u = v = 0.
NF_DEV int nf_wofz(double xi, double yi, double* uo, double* vo) {
  const double FACTOR = 1.12837916709551257388, RMAXREAL = 0.5e154, RMAXEXP = 708.503061461606, RMAXGONI = 3.53711887601422e15;
  const double xabs = fabs(xi), yabs = fabs(yi), x = xabs/(double)6.3f, y = yabs/(double)4.4f;
  double u = 0.0, v = 0.0, u2 = 0.0, v2 = 0.0;
  *uo = 0.0; *vo = 0.0;
  if (xabs > RMAXREAL || yabs > RMAXREAL) return 1;
  double qrho = x*x + y*y;
  const double xabsq = xabs*xabs;
  double xquad = xabsq - yabs*yabs;
  const double yquad = 2*xabs*yabs;
  const int a = qrho < 0.085264;
  if (a) {
    qrho = (1 - (double)0.85f*y)*sqrt(qrho);
    const int n = (int)floor(6 + 72*qrho + 0.5);
    int j = 2*n + 1;
    double xsum = (double)(1.0f/(float)j), ysum = 0.0;
    for (int i = n; i >= 1; --i) {
      j = j - 2;
      const double xaux = (xsum*xquad - ysum*yquad)/i;
      ysum = (xsum*yquad + ysum*xquad)/i;
      xsum = xaux + (double)(1.0f/(float)j);
    }
    const double u1 = -FACTOR*(xsum*yabs + ysum*xabs) + 1.0;
    const double v1 =  FACTOR*(xsum*xabs - ysum*yabs);
    const double daux = exp(-xquad);
    u2 =  daux*cos(yquad);
    v2 = -daux*sin(yquad);
    u = u1*u2 - v1*v2;
    v = u1*v2 + v1*u2;
  } else {
    double h, h2 = 0.0, qlambda = 0.0; int kapn, nu;
    if (qrho > 1.0) {
      h = 0.0; kapn = 0; qrho = sqrt(qrho);
      nu = (int)(3 + (1442/(26*qrho + 77)));
    } else {
      qrho = (1 - y)*sqrt(1 - qrho);
      h = (double)1.88f*qrho; h2 = 2*h;
      kapn = (int)floor(7 + 34*qrho + 0.5);
      nu   = (int)floor(16 + 26*qrho + 0.5);
    }
    const int b = h > 0.0;
    if (b) qlambda = nf_powi(h2, kapn);
    double rx = 0.0, ry = 0.0, sx = 0.0, sy = 0.0;
    for (int n = nu; n >= 0; --n) {
      const int np1 = n + 1;
      double tx = yabs + h + np1*rx;
      const double ty = xabs - np1*ry;
      const double c = 0.5/(tx*tx + ty*ty);
      rx = c*tx; ry = c*ty;
      if (b && n <= kapn) { tx = qlambda + sx; sx = rx*tx - ry*sy; sy = ry*tx + rx*sy; qlambda = qlambda/h2; }
    }
    if (h == 0.0) { u = FACTOR*rx; v = FACTOR*ry; } else { u = FACTOR*sx; v = FACTOR*sy; }
    if (yabs == 0.0) u = exp(-xabs*xabs);
  }
  if (yi < 0.0) {
    if (a) { u2 = 2*u2; v2 = 2*v2; }
    else {
      xquad = -xquad;
      if (yquad > RMAXGONI || xquad > RMAXEXP) return 1;
      const double w1 = 2*exp(xquad);
      u2 =  w1*cos(yquad);
      v2 = -w1*sin(yquad);
    }
    u = u2 - u; v = v2 - v;
    if (xi > 0.0) v = -v;
  } else {
    if (xi < 0.0) v = -v;
  }
  *uo = u; *vo = v; return 0;
}
NF_DEV double WofzRe(double zr, double zi) { double u, v; nf_wofz(zr, zi, &u, &v); return u; }
// Voigt profile as internal_funcs.f90 (VOIGT_IF) and USERFCN.f (VOIGT) define it.
NF_DEV double VOIGT_IF(double x, double x0, double amp, double sigma, double gamma) {
  return amp*WofzRe((x - x0)/(sigma*sqrt(2.0)), gamma/(sigma*sqrt(2.0)))/(sigma*sqrt(2.0*NF_PI));
}
