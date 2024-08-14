PROGRAM STR_TEST
    USE MOD_UTEST
    USE MOD_STRUTIL

    ! Test escape char substitution
    CALL STR_ESCAPE()

    CONTAINS

    SUBROUTINE STR_ESCAPE()
        CHARACTER(512) :: subs_test, out

        CALL BEGIN_TEST('STR_ESCAPE')

        subs_test = 'escape\these\chars\\when\needed'
        out = ' '
        CALL STR_ESCAPE_CHARS(subs_test, '\', out)
        WRITE(*,*) out

        CALL ASSERT_REQUIRED((TRIM(out).EQ.'escape\\these\\chars\\\\when\\needed'), 'Strings do not match.')
        
        CALL END_TEST()
    END SUBROUTINE
END PROGRAM STR_TEST
