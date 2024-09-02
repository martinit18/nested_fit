# Small guide of the examples

 Examples of nested fit input files for data analysis are presented in the folder `data_analysis`. In particular
- in `aaa_simple_example` is the **simples** example anyone can start for testing the program,
- in `search_methods` different **search methods** are presented for counts data, 
- in `error_bars_data` an example is given for data with error bars and assuming a Gaussian probability for them, 
- in `robust_analysis` an example is given for data with error bars with outliers and assuming a Jeffreys probability for them, 
- in `data_2D` an example of **2D data** is given,
- in `set_of_datafile` an example of **simultaneous analysis of set data files** is given.

Examples of nested fit input files for function exploration are presented in the folder `function_exploration`. In particular
- in `func_EGGBOX` different **clustering methods** are presented,
- in `func_ENERGY_HARM_3D` an analysis of **partition function** with a choice of minimum temperature is given.

Examples of use of the python library `nested_res.py` are presented in two jupyter notebooks
- `simple_analysis_visu.ipynb` for a simple visualization of `nested_fit` outputs for a single analysis
- `pandas_extended_analysis.ipynb` for a more **complex analysis** for different choice of model, conditions or data set. Here, the output results are collected in a unique pandas dataframe to be easily compared and visualized 