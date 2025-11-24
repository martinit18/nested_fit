# Step-by-step installation instruction

## For Mac OS
1) If you do not have a fortran compiler installed, install homebrew from [https://brew.sh/](https://brew.sh/)\
Then in a terminal install with homebrew the required components:\
 `brew install gcc gfortran make cmake`

If you did not do it yet, add homebrew binary directory in the shell variable `PATH` by adding **IN THIS ORDER** in `.bashrc`: 
```sh
export PATH=/opt/homebrew/bin:/opt/homebrew/opt/make/libexec/gnubin:$PATH
```
(adapt it if needed).
The order force to override the default binaries by the homebrew binaries and thus take `gcc` from homebrew and not the Mac OS preinstalled one.


2) If you do not have python installed (the native python with Mac OS is generally outdated), install with\
 `brew install python`

Installation of the main code and python package dependencies can be found at the end of the document, being OS independent.

## For Windows
1) If you do not have a virtual machine, install it from the app Microsoft store with the Window Subsystem for Linux. 
    - Go to the store
    - Find *Ubuntu*, choose the distribution you like and install it
    (if you have some troubles for this part, have a look to specific helps [here](https://learn.microsoft.com/en-us/windows/wsl/install) as example)
2) Open a linux emulator terminal
3) Follow the linux installation instruction here below


## For Linux 

1)  If you do not have a fortran compiler installed, install it with\
`sudo apt install gfortran make cmake`\
(or `sudo apt-get install gfortran make cmake`)

## Main code and dependencies installation

This installation assumes that you have bash as shell.

1) In the terminal, install the requested python packages, if not already installed, with\
 `pip install numpy scipy matplotlib ipython jupyter pandas getdist anesthetic`
2) Choose a directory and go on it
3) Expand the zip file [here](https://github.com/martinit18/nested_fit/tags) or clone the program with git with\
`git clone https://github.com/martinit18/nested_fit`
4) create the directory `bin` (`mkdir bin`) in your home (normally the place where you are when you open a new terminal)
5) **obsolete, see main README file for this part** \
go to the decompressed directory and create the directory `build`:\
`cd nested_fit-xxx` (where `xxx` is the present version number)\
`mkdir build`\
`cd build`
6) Go  to the directory build and run\
`cmake ..`\
and then\
 `make`
7) In the file `.bashrc` in your home put the line\
`export PATH=$HOME/bin:$PATH` 

Now if everything works, you can run `nested_fit` from terminal anywere you are.

## Testing the code

 - Go to the distribution directory `examples/data_analysis/gauss_bg`
 - Run `nested_fitxxx` (where `xxx` is the present version number) 
 - If everything works, the program will run and produce a series of `nf_output_*` files including `nf_output_res.dat`

More details on the program itself can be found in the `README` file.