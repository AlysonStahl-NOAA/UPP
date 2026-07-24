! This is a test program for UPP.
!
! This program tests the subroutines in the xml_perl_data module.
!
! Alyson Stahl, 7/2026
program test_xml_perl_data
    use xml_perl_data
    use rqstfld_mod, only: num_post_afld
    use CTLBLK_mod, only: tprec, tclod, trdlw, trdsw, tsrfc, tmaxmin, td3d, filenameflat
    implicit none

    integer, parameter :: ntests = 3 ! This will also be the same 
    integer :: i, j, res
    character :: inpchar
    !
    integer :: EXP_NUM_POST_AFLD, EXP_PARAMSET_COUNT
    real :: EXP_TPREC

    ! XML file name
    filenameflat = "data/ref_test_xml_perl_data.txt"

    print *, "Testing filter_char_inp() subroutine..."

    ! Test Case 1: Input is "?", should return empty string.
    inpchar = "?"
    call filter_char_inp(inpchar)

    if (inpchar .ne. " ") then
        print *, "Test Case 1 Failed: Input is '?', Result: ", inpchar
        stop 10
    end if

    ! Test Case 2: Input should remain unchanged.
    inpchar = "a"
    call filter_char_inp(inpchar)
    if (inpchar .ne. "a") then
        print *, "Test Case 2 Failed: Input is 'a', Result: ", inpchar
        stop 20
    end if

    print *, "Testing read_postxconfig() subroutine..."
    
    ! These variables only used in gen_proc_type == 'ens_fcst' case.
    tprec = 2.
    tclod = 1
    trdlw = 1.
    trdsw = 1.
    tsrfc = 1.
    tmaxmin = 1.
    td3d = 1.

    call read_postxconfig()
    
        if (tprec .ne. EXP_TPREC .OR. tclod .ne. EXP_TPREC .OR. trdlw .ne. EXP_TPREC &
            .OR. trdsw .ne. EXP_TPREC .OR. tsrfc .ne. EXP_TPREC .OR. tmaxmin .ne. EXP_TPREC &
            .OR. td3d .ne. EXP_TPREC) then
        print *, "Test Failed: One or more parameters did not match expected values."
        stop 30
    end if

    print *, "SUCCESS!"
end program test_xml_perl_data