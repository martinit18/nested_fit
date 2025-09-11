#!/bin/bash

# nested_fit installer script
# César Godinho, 11/09/2025

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
echo ":: $(gct) :: Installing required packages..."

# if [ "$EUID" -ne 0 ]; then
# 	echo "This script requires elevation to run."
# 	exit 1
# fi

if $INSTALL_DEPS; then
	if command -v apt-get > /dev/null; then
		sudo apt update
		sudo apt install -y build-essential cmake make git gcc g++ gfortran python3 python3-pip
	elif command -v dnf > /dev/null; then
		sudo dnf install -y cmake make git gcc g++ gfortran python3 python3-pip
	elif command -v yum > /dev/null; then
		sudo yum install -y cmake make git gcc g++ gfortran python3 python3-pip
	elif command -v pacman > /dev/null; then
		sudo pacman -Sy --noconfirm cmake make git gcc g++ gfortran python3 python3-pip
	elif command -v apk > /dev/null; then
		sudo apk add cmake make git gcc g++ gfortran python3 python3-pip
	else
		echo "Could not install requirements via package manager..."
		echo "Please install cmake, make, git, gcc, g++, gfortran, python3."
		echo "Then launch this script with the --no-deps flag."
		exit 1
	fi
fi

INSTALL_CMD=""

if $USE_VENV; then
	INSTALL_CMD="python3 -m pip install --user ./nested_fit -v"

	echo ":: $(gct) :: Creating virtual environment..."
	python3 -m venv .nf_venv
	source .nf_venv/bin/activate
else
	INSTALL_CMD="python3 -m pip install ./nested_fit -v"

	echo ":: $(gct) :: Installing to the current user..."
	echo ":: $(gct) :: If you prefer you can install into a venv at the current directory by using"
	echo ":: $(gct) :: the --use-venv flag."
fi

echo ":: $(gct) :: Updating pip..."
python3 -m pip install --upgrade pip

if [[ $BRANCH -e "dev" ]]; then
	echo -e "\e[33m:: $(gct) :: Warning: Installing nightly dev branch...\e[0m"
fi
echo ":: $(gct) :: Cloning repository..."
git clone --single-branch --branch $BRANCH https://github.com/martinit18/nested_fit.git
mkdir -p nested_fit/build

echo ":: $(gct) :: Installling nested_fit..."
$INSTALL_CMD
