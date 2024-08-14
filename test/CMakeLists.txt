project(nested_fit_test LANGUAGES Fortran)

add_executable(nf_test_str 
	test.f90
	str.f90
	../src/helper/strutil.f90
)
add_test(NAME nf_test_str COMMAND nf_test_str)
set_property(TEST nf_test_str PROPERTY
	PASS_REGULAR_EXPRESSION "Status.*:.*PASSED"
)
