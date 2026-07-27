! This is a test program for UPP.
!
! This program tests the READ_xml() subroutine.
!
! Alyson Stahl, 7/2026

program test_read_xml
    use READ_XML_UPP_MOD
    use xml_perl_data, only: READ_xml
    use grib2_module, only: num_pset
    use rqstfld_mod, only: num_post_afld
    use CTLBLK_mod, only: filenameflat
    implicit none

    integer :: res
    integer :: EXP_NUM_PSET, EXP_NUM_POST_AFLD

    ! Also used in test_xml_perl_data.f90
    filenameflat = "data/ref_test_xml_perl_data_case2.txt"

    call READ_xml()

    print *, num_pset
    print *, num_post_afld

    print *, "SUCCESS!"
end program test_read_xml