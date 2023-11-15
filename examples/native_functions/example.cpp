// Brief : Example c++ function to RT into nested_fit
// Author: César Godinho
// Date  : 15/11/23

// These defines are required and must be empty, the argument names can be arbitrary
#define DCL_VAR(name, index)
#define NF_FUNC_EXPORT

// You can use any c++11 compliant code
#include <tuple>
#include <utility>
#include <iostream>
#include <limits>
#include <cmath> 

// E.g. Linear function with params (using a tuple)
using DoublePair = std::pair<double, double>;
static double linear(const double x, const DoublePair& params)
{
    return params.first * x + params.second;
}

// Even classes work
// Any c++ compliant code really
class LinearFunc
{
public:
    LinearFunc(double a, double b) : a(a), b(b) {  }
    ~LinearFunc() = default;

    double operator()(double x) const
    {
        return linear(x, std::make_pair(a, b));
    }

private:
    double a, b;
};

class QuadraticFunc
{
public:
    QuadraticFunc(double a, double b, double c) : a(a), b(b), c(c) {  }
    ~QuadraticFunc() = default;

    double operator()(double x) const
    {
        return a * x * x + b * x + c;
    }

    LinearFunc diff() const
    {
        return LinearFunc(a, b);
    }

    DoublePair isect(const QuadraticFunc& other) const
    {
        double _a = a - other.a;
        double _b = b - other.b;
        double _c = c - other.c;

        // It's dangerous to go alone
        // Don't divide by 0
        if(std::abs(_a) < std::numeric_limits<double>::epsilon())
        {
            return std::make_pair(-_c/_b, std::numeric_limits<double>::quiet_NaN());
        }

        double disc = _b * _b - 4 * _a * _c;
        double nan = std::numeric_limits<double>::quiet_NaN();
        if(disc < 0.0) return std::make_pair(nan, nan);

        std::cout << disc << std::endl;

        double s0 = (-_b + std::sqrt(disc)) / (2.0 * _a);
        double s1 = (-_b - std::sqrt(disc)) / (2.0 * _a);

        return std::make_pair(s0, s1);
    }

private:
    double a, b, c;
};

// Now declare the export function via the NF_FUNC_EXPORT directive
// extern "C" is also required
// The Function signature needs to be exactly like this (except parameter names)
NF_FUNC_EXPORT extern "C" double export_func(double* x, int* npar, double* params)
{
    // This can be anywhere inside the function
    // This assigns the parameter names to the params
    // Usually you'll want to name as many as you use
    // Since this will be used to calculate the npar
    // These are the names that must be present in the input file
    DCL_VAR("x_0", 0);
    DCL_VAR("x_1", 1);
    
    // This is possible but not recomended since, this function will be called
    // many, many times from within nested_fit
    // Here it is just illustratory
    std::cout << "Hello from c_func" << std::endl;
    
    QuadraticFunc f0(1, 0, params[0]); // f0(x) = x^2 + x_0
    QuadraticFunc f1(1, 2, params[1]); // f1(x) = x^2 + 2x + x_1

    auto isect = f0.isect(f1); // f0 = f1
    
    LinearFunc f2(1, *x); // f2(x) = x + (*x [our x])
    
    // result = f2(f0'(f0 = f1))
    double result = f2(f0.diff()(isect.first));

    return result;
}

// Now just test it:
// nested_fitx.x.x -fa example.cpp
// nested_fitx.x.x -fr 'export_func(2, 2, [-4, -3])'

