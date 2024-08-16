### How to use the simple unit test suit

1. Add a new test on the CMakeLists.txt file under this directory. (This really should be macro'ed).
```cmake
add_executable(nf_<test_name>
    <test_source>
    ../src/<test_source_deps>
)
add_test(NAME nf_<test_name> COMMAND nf_<test_name>)
set_property(TEST nf_<test_name> PROPERTY
    PASS_REGULAR_EXPRESSION "Status.*:.*PASSED"
)
```

2. Write your test on the `<test_source>` source file. For an example you can look under the `str.f90` file.
Important to use the `BEGIN_TEST`/`END_TEST` subroutines.

3. Compile normally. To run tests either (in build dir): `ctest` or `make test`.


### Other important aspects

- The subroutine `ASSERT_REQUIRED` takes in a `LOGICAL` and an error message string. The test will fail if the logical value is `.FALSE.`.
- One might call `ASSERT_REQUIRED` multiple times.
- Try and keep a single test file as specific as posssible (as for example in `str.f90`).
- If one wants to look at the test std output, there are two main options.
    1. Look into `Testing/Temporary/LastTest.log` for the stdout of all tests.
    2. Run the test using: `ctest -R <test_name> -V`.
- To list all tests: `ctest -N`.
- For a more detailed usage of `ctest`, view `ctest -h`.
