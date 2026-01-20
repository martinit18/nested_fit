# Small guide of the examples

 Examples of nested fit input files for data analysis are presented in the folder `data_analysis`. In particular
- in `01_simple_example` is the **simples** example anyone can start for testing the program,
- in `02_error_bars_data` an example is given for data with error bars and assuming a Gaussian probability for them, 
- in `03_with_cluster_analysis` different cluster analyses are presented,
- in `04_set_of_datafile` an example of **simultaneous analysis of set data files** is given,
- in `05_data_2D` an example of **2D data** is given,
- in `06_interpolation_function` the interpolation method is presented  (in a Jupyter notebook for practical reasons)

Examples of nested fit input files for function exploration are presented in the folder `function_integration`: a Gaussian function in 5D `07_func_GAUSS` (the integral is 1 -> 0 in log) and a eggbox function for testing the clustering abilities `08_func_EGGBOX`.

Examples of nested fit input files for potential exploration and analysis of the **partition function** with a choice of minimum temperature is given in the folder `potential_exploration`. Two Lennard-Jones clusters are given as example: a cluster with 7 classic atoms (`09_ENERGY_HARM_3D`) and a cluster with 3 atoms with nuclear quantum effects emulated with two replicas (`10_Q_ENERGY_HARM_3D`).


Examples of use of the python library `nested_res.py` are presented in two jupyter notebooks
- `simple_analysis_visu.ipynb` for a basic analysis and visualization of `nested_fit` outputs for a single analysis.
- `simple_visualisation.ipynb` for basic visualisation of already computed analyses.
- `set_of_files_analysis.ipynb` for the analysis of two spectra at the same time with two different, but correlated, functions.
- `pandas_extended_analysis.ipynb` for a more **complex analysis** for different choice of model, conditions or data set. Here, the output results are collected in a unique pandas dataframe to be easily compared and visualized 

Another notebook can be found in `examples/data_analysis/06_interpolation_function/pynf_interp.ipynb` for interpolation of external data.

Numbered folders are referring to reference test benchmarks used to develop the code.