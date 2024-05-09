PROGRAM strsplit_test
    USE MOD_STRUTIL
    CHARACTER(1024) :: string
    CHARACTER(1024), DIMENSION(16) :: split
    INTEGER :: count

    ! Test normal split
    string = 'split with spaces'
    CALL SPLIT_INPUT_ON(' ', TRIM(string), split, count, 16)

    IF(count.NE.3) CALL EXIT(1)
    IF(TRIM(split(1)).NE.'split')  CALL EXIT(2)
    IF(TRIM(split(2)).NE.'with')   CALL EXIT(3)
    IF(TRIM(split(3)).NE.'spaces') CALL EXIT(4)

    ! Test repeat split
    string = 'split   with         multiple spaces'
    CALL SPLIT_INPUT_ON(' ', TRIM(string), split, count, 16, repeat = .TRUE.)

    IF(count.NE.4) CALL EXIT(1)
    IF(TRIM(split(1)).NE.'split')  CALL EXIT(2)
    IF(TRIM(split(2)).NE.'with')   CALL EXIT(3)
    IF(TRIM(split(3)).NE.'multiple')   CALL EXIT(4)
    IF(TRIM(split(4)).NE.'spaces') CALL EXIT(5)

    ! Test normal multi-char split
    string = 'split with spaces,and,commas'
    CALL SPLIT_INPUT_ON_V(' ,', TRIM(string), split, count, 16)

    IF(count.NE.5) CALL EXIT(1)
    IF(TRIM(split(1)).NE.'split')  CALL EXIT(2)
    IF(TRIM(split(2)).NE.'with')   CALL EXIT(3)
    IF(TRIM(split(3)).NE.'spaces') CALL EXIT(4)
    IF(TRIM(split(4)).NE.'and') CALL EXIT(5)
    IF(TRIM(split(5)).NE.'commas') CALL EXIT(6)

    ! Test repeat multi-char split
    string = 'split with    multiple, spaces  , and,,,multiple,commas'
    CALL SPLIT_INPUT_ON_V(' ,', TRIM(string), split, count, 16, repeat = .TRUE.)

    IF(count.NE.7) CALL EXIT(1)
    IF(TRIM(split(1)).NE.'split')    CALL EXIT(2)
    IF(TRIM(split(2)).NE.'with')     CALL EXIT(3)
    IF(TRIM(split(3)).NE.'multiple') CALL EXIT(4)
    IF(TRIM(split(4)).NE.'spaces')   CALL EXIT(5)
    IF(TRIM(split(5)).NE.'and')      CALL EXIT(6)
    IF(TRIM(split(6)).NE.'multiple') CALL EXIT(7)
    IF(TRIM(split(7)).NE.'commas')   CALL EXIT(8)

    ! .tsv allowing spaces and tabs example
    string = 'split'//CHAR(9)//'like    mixed    '//CHAR(9)//'.tsv'
    CALL SPLIT_INPUT_ON_V(' '//CHAR(9), TRIM(string), split, count, 16, repeat = .TRUE.)

    IF(count.NE.4) CALL EXIT(1)
    IF(TRIM(split(1)).NE.'split')    CALL EXIT(2)
    IF(TRIM(split(2)).NE.'like')     CALL EXIT(3)
    IF(TRIM(split(3)).NE.'mixed') CALL EXIT(4)
    IF(TRIM(split(4)).NE.'.tsv')   CALL EXIT(5)
    
    CALL EXIT(0)
END PROGRAM
