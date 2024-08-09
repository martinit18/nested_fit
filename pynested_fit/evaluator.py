# Brief  : Loads nested_fit's shared object containing user/system functions.
#          Allowing to evaluate the function at given params.
#          Note that this is limited to non-legacy functions.
# Author : César Godinho
# Date   : 09/08/2024

from ctypes import cdll, c_double, c_int, POINTER, RTLD_GLOBAL, CDLL, byref
import numpy as np # We need numpy for other parts of pynested_fit anyways
import numpy.typing as npt
import platform
from .metadata import __cache__

class NFEvaluator():
    def __init__(self, func_name: str):
        self._fname = func_name

        if platform.system() == 'Linux' or platform.system() == 'Darwin':
            self._gfortran_lib = CDLL('libgfortran.so', mode = RTLD_GLOBAL)
            self._nf_func_lib = CDLL(f'{__cache__}/dynamic_calls.so', mode = RTLD_GLOBAL)
        elif platform.system() == 'Windows':
            self._nf_func_lib = CDLL(f'{__cache__}/dynamic_calls.dll') # HACK: (César) Not sure about this for Windows...
        else:
            self._f = None
            return

        self._f = getattr(self._nf_func_lib, func_name)

        # Same as the c declaration: double <fname>(double*, int*, double*)
        self._f.restype  = c_double
        self._f.argtypes = [POINTER(c_double), POINTER(c_int), np.ctypeslib.ndpointer(dtype=np.float64, ndim=1, flags='C_CONTIGUOUS')]

    def is_valid(self):
        # Unless the user is accessing this freely,
        # this should basically be equivalent of checking
        # if the current functions is a legacy function or not.
        return self._f is not None

    def at(self, x: float, params: npt.NDArray[np.float64]) -> np.float64 | None:
        if self._f:
            return np.float64(self._f(byref(c_double(x)), byref(c_int(params.size)), params))
        return None

    def atv(self, X: npt.NDArray[np.float64], params: npt.NDArray[np.float64]) -> npt.NDArray[np.float64] | None:
        if self._f:
            return np.array([np.float64(self._f(byref(c_double(x)), byref(c_int(params.size)), params)) for x in X], dtype=np.float64)
        return None
