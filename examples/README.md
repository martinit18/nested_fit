# Small guide of the examples

 Examples of nested fit input files for data analysis are presented in the folder `data_analysis`. In particular
- in `01_simple_example` is the **simples** example anyone can start for testing the program,
- in `02_error_bars_data` an example is given for data with error bars and assuming a Gaussian probability for them, 
- in `03_with_cluster_analysis` different cluster analyses are presented,
- in `04_set_of_datafile` an example of **simultaneous analysis of set data files** is given,
- in `05_data_2D` an example of **2D data** is given,
- in `06_interpolation_function' the interpolation method is presented  (in a Jupyter notebook for practical reasons)

Examples of nested fit input files for function exploration are presented in the folder `function_integration`. 

Examples of nested fit input files for potential exploration and analysis of the **partition function** with a choice of minimum temperature is given in the folder `potential_exploration`.


Examples of use of the python library `nested_res.py` are presented in two jupyter notebooks
- `simple_analysis_visu.ipynb` for a basic analysis and visualization of `nested_fit` outputs for a single analysis
- `pandas_extended_analysis.ipynb` for a more **complex analysis** for different choice of model, conditions or data set. Here, the output results are collected in a unique pandas dataframe to be easily compared and visualized 

Numbered folders are referring to reference test benchmarks used to develop the code.