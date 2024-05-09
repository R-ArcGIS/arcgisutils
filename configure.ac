AC_INIT

# Export PATH to include Cargo binary directory
export PATH="$PATH:$HOME/.cargo/bin"

# Check for Rustc
AC_CHECK_PROGS([RUSTC], [rustc], [no])
if test "$RUSTC" = "no"; then
    echo "----------------------- [RUST NOT FOUND]---------------------------"
    echo "The 'rustc' compiler was not found on the PATH. Please install Rust"
    echo "from: https://www.rust-lang.org/tools/install"
    echo ""
    echo "Alternatively, you may install Rust from your OS package manager:"
    echo " - Debian/Ubuntu: apt-get install rustc"
    echo " - Fedora/CentOS: dnf install rustc"
    echo " - macOS: brew install rustc"
    echo "-------------------------------------------------------------------"
    echo ""
    exit 1
fi

# Check if Cargo is installed
if ! command -v cargo &> /dev/null; then
    echo "----------------------- [CARGO NOT FOUND]--------------------------"
    echo "The 'cargo' command was not found on the PATH. Please install Cargo"
    echo "from: https://www.rust-lang.org/tools/install"
    echo ""
    echo "Alternatively, you may install Cargo from your OS package manager:"
    echo " - Debian/Ubuntu: apt-get install cargo"
    echo " - Fedora/CentOS: dnf install cargo"
    echo " - macOS: brew install rustc"
    echo "-------------------------------------------------------------------"
    echo ""
    exit 1
fi

AC_OUTPUT
# Check for Rscript
# if test -z "$R_HOME"; then
#     echo "----------------------- [R_HOME NOT SET]------------------------"
#     echo "The 'R_HOME' environment variable is not set. Please make sure R"
#     echo "is installed and set the R_HOME environment variable."
#     echo "-------------------------------------------------------------------"
#     echo ""
#     exit 1
# fi

# RSCRIPT="${R_HOME}/bin/Rscript"

# # Check if Rscript is executable
# if ! test -x "$RSCRIPT"; then
#     echo "----------------------- [RSCRIPT NOT EXECUTABLE]------------------------"
#     echo "The 'Rscript' executable is not found or not executable. Please make sure R"
#     echo "is installed correctly and 'Rscript' is executable."
#     echo "--------------------------------------------------------------------------"
#     echo ""
#     exit 1
# fi

# Output status
# echo "Rust is installed, Cargo is found, R_HOME is set, and Rscript is found."
# echo "Running tools/get-deps.R script..."

# # Execute the get-deps.R script
# if ! "$RSCRIPT" ./tools/get-deps.R; then
#     echo "Error: Failed to execute tools/get-deps.R."
#     exit 1
# fi
