# Step-by-step installation instruction

## For Mac OS
1) If you do not have a fortran compiler installed, install homebrew from [https://brew.sh/](https://brew.sh/)\
Then in a terminal install with homebrew the required components:\
 `brew install gcc make cmake`

2) If you do not have python installed (the native python with Mac OS is generally outdated), install with\
 `brew install python`

Installation of the main code and python package dependencies can be found at the end of the document, being OS independent.

## For Windows
1) If you do not have a virtual machine, install it from the app Microsoft store with the Window Subsystem for Linux. 
    - Go to the store
    - Find *Ubuntu*, choose the distribution you like and install it
1) Open a linux emulator terminal
1) Follow the linux installation instruction here below


## For Linux

1)  If you do not have a fortran compiler installed, install it with\
`sudo apt install gfortran make cmake`\
(or `sudo apt-get install gfortran make cmake`)

## Main code and dependencies installation

This installation assumes that you have bash as shell.

1) In the terminal, install the requested python packages, if not already installed, with\
 `pip install numpy scipy matplotlib ipython jupyter pandas getdist anesthetic`
2) Choose a directory and go on it
3) Clone the program with\
`git clone https://github.com/martinit18/nested_fit`
4) create a directory `bin` in your home (normally the place where you are when you open a new terminal)
5) Go  to the directory build and run\
`cmake ..`\
and then\
 `make`
6) In the file `.bashrc` in your home put the line\
`export PATH=$HOME/bin:$PATH`

Now if everything works, you can run `nested_fit` from terminal anywere you are.

