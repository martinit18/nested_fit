#!/bin/bash
# Brief  : nested_fit installer script
# Author : César Godinho
# Date	 : 11/09/2025

set -e

gct() {
	date +"%H:%M:%S"
}

# ARGS
INSTALL_DEPS=true
BRANCH="master"
USE_VENV=false

# Parse ARGS
while [[ $# -gt 0 ]]; do
	case "$1" in
		--no-deps)
			INSTALL_DEPS=false
			shift
		;;

		--nightly)
			BRANCH="dev"
			shift
		;;

		--use-venv)
			USE_VENV=true
			shift
		;;

		*)
			echo "Unknown option: $1"
			exit 1
		;;
	esac
done

# Detect OS
OS="$(uname -s)"
case "$OS" in
	Linux*)  PLATFORM="linux" ;;
	Darwin*) PLATFORM="macos" ;;
	*) 		 echo "Unsupported OS: $OS" ; exit 1 ;;
esac

echo ":: $(gct) :: Installing nested_fit for $(uname -o) $(uname -m)..."

if [[ "$PLATFORM" == "macos" ]]; then
	if $INSTALL_DEPS; then
		echo ":: $(gct) :: Installing required packages..."

		if command -v brew > /dev/null; then
			brew install gcc g++ gfortran git cmake make python3 pipx

			# Make sure we have brew on path
			export PATH=/opt/homebrew/bin:/opt/homebrew/make/libexec/gnubin:$PATH
		else
			echo "Brew is required to install dependencies on macos."
			echo "Please download brew or install cmake, make, git, gcc, g++, gfortran, python3, python3-pip, pipx."
			echo "Then launch this script with the --no-deps flag."
			exit 1
		fi
	fi
else
	if $INSTALL_DEPS; then
		echo ":: $(gct) :: Installing required packages..."
		echo ":: $(gct) :: Elevated console is required. If not wanted, run this script with the --no-deps flag."

		if command -v apt-get > /dev/null; then
			sudo apt update
			sudo apt install -y build-essential cmake make git gcc g++ gfortran python3 python3-pip pipx
		elif command -v dnf > /dev/null; then
			sudo dnf install -y build-essential cmake make git gcc g++ gfortran python3 python3-pip pipx
		elif command -v yum > /dev/null; then
			sudo yum install -y build-essential cmake make git gcc g++ gfortran python3 python3-pip pipx
		elif command -v pacman > /dev/null; then
			sudo pacman -Sy --noconfirm build-essential cmake make git gcc g++ gfortran python3 python3-pip pipx
		elif command -v apk > /dev/null; then
			sudo apk add build-essential cmake make git gcc g++ gfortran python3 python3-pip pipx
		else
			echo "Could not install requirements via package manager..."
			echo "Please install cmake, make, git, gcc, g++, gfortran, python3."
			echo "Then launch this script with the --no-deps flag."
			exit 1
		fi
	fi
fi

INSTALL_CMD=""

if $USE_VENV; then
	INSTALL_CMD="python3 -m pip install ./nested_fit -v"

	echo ":: $(gct) :: Creating virtual environment..."
	python3 -m venv .nf_venv
	source .nf_venv/bin/activate
else
	INSTALL_CMD="python3 -m pipx install ./nested_fit -v"

	echo ":: $(gct) :: Installing to the current user using pipx..."
	echo ":: $(gct) :: If you prefer you can install into a venv at"
	echo ":: $(gct) :: the current directory by using the --use-venv flag."
fi

if [[ "$BRANCH" == "dev" ]]; then
	echo -e "\e[33m:: $(gct) :: Warning: Installing nightly dev branch...\e[0m"
	echo -e "\e[33m:: $(gct) :: Warning: This is discouraged unless you want some nightly features...\e[0m"
fi
echo ":: $(gct) :: Cloning repository..."
git clone --single-branch --branch $BRANCH https://github.com/martinit18/nested_fit.git

if [[ "$PLATFORM" == "macos" ]]; then
	# On MacOS make sure we are using the highest priority compilers
	IFC=$(which gfortran)
	ICC=$(which gcc)
	ICXX=$(which g++)

	mkdir -p nested_fit/build
	cmake -S nested_fit -B nested_fit/build -DCMAKE_Fortran_COMPILER=$IFC -DCMAKE_C_COMPILER=$ICC -DCMAKE_CXX_COMPILER=$ICXX -DOPENMP=ON -DCMAKE_BUILD_TYPE=Release 
fi

echo ":: $(gct) :: Installing nested_fit..."
$INSTALL_CMD
