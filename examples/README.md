# Small guide of the examples

 Examples of nested fit input files for data analysis are presented in the folder `data_analysis`. In particular
- in `synthetic_gauss_bg_low-statistics` different **search methods** are presented, 
- in ``synthetic_gauss_bg_set`` an example of **simultaneous analysis of set data files** is give.

Examples of nested fit input files for function exploration are presented in the folder `function_exploration`. In particular
- in `func_EGGBOX` different **clustering methods** are presented,
- in `func_ENERGY_HARM_3D` an analysis of **partition function** with a choice of minimum temperature is given.

Examples of use of the python library `nested_res_Vxx.py` are presented in two jupyther notebooks
-  `simple_analysis_visu.ipynb` for a simple visualization of `nested_fit` outputs for a single analysis
- `pandas_extended_analysis.ipynb` for a more **complex analysis** for different choice of model, conditions or data set. Here, the output results are collected in a unique pandas dataframe to be easily compared and visualized 