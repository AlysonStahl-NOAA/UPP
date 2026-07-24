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

    integer, parameter :: ntests = 3 
    real, parameter :: tol = 1.0e-5
    !
    integer :: i, j, res
    character :: inpchar
    !
    integer :: EXP_NUM_POST_AFLD, EXP_PARAMSET_COUNT
    real :: EXP_TPREC
    type(paramset_t) :: EXP_PARAMSET(ntests)

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
    
    EXP_TPREC = 2.0
    EXP_NUM_POST_AFLD = 2
    EXP_PARAMSET_COUNT = ntests

    ! Test Case 1: param_count = 0
    EXP_PARAMSET(1)%datset = "ps0"
    EXP_PARAMSET(1)%grid_num = 1
    EXP_PARAMSET(1)%sub_center = "sc0"
    EXP_PARAMSET(1)%version_no = "v0"
    EXP_PARAMSET(1)%local_table_vers_no = "lt0"
    EXP_PARAMSET(1)%sigreftime = "sr0"
    EXP_PARAMSET(1)%prod_status = "ps0"
    EXP_PARAMSET(1)%data_type = "dt0"
    EXP_PARAMSET(1)%gen_proc_type = "fcst"
    EXP_PARAMSET(1)%time_range_unit = "hour"
    EXP_PARAMSET(1)%orig_center = "oc0"
    EXP_PARAMSET(1)%gen_proc = "gp0"
    EXP_PARAMSET(1)%packing_method = "pm0"
    EXP_PARAMSET(1)%field_datatype = "fd0"
    EXP_PARAMSET(1)%comprs_type = "ct0"
    allocate(EXP_PARAMSET(1)%param(0))

    ! Test Case 2: One parameter has cc = cv = level_array_count = level2_array_count = scale_array_count = 0
    EXP_PARAMSET(2)%datset = "ps1"
    EXP_PARAMSET(2)%grid_num = 2
    EXP_PARAMSET(2)%sub_center = "sc1"
    EXP_PARAMSET(2)%version_no = "v1"
    EXP_PARAMSET(2)%local_table_vers_no = "lt1"
    EXP_PARAMSET(2)%sigreftime = "sr1"
    EXP_PARAMSET(2)%prod_status = "ps1"
    EXP_PARAMSET(2)%data_type = "dt1"
    EXP_PARAMSET(2)%gen_proc_type = "fcst"
    EXP_PARAMSET(2)%time_range_unit = "hour"
    EXP_PARAMSET(2)%orig_center = "oc1"
    EXP_PARAMSET(2)%gen_proc = "gp1"
    EXP_PARAMSET(2)%packing_method = "pm1"
    EXP_PARAMSET(2)%field_datatype = "fd1"
    EXP_PARAMSET(2)%comprs_type = "ct1"
    allocate(EXP_PARAMSET(2)%param(1))
    EXP_PARAMSET(2)%param(1)%post_avblfldidx = 101
    EXP_PARAMSET(2)%param(1)%shortname = "sn1"
    EXP_PARAMSET(2)%param(1)%longname = "longname1"
    EXP_PARAMSET(2)%param(1)%pname = "pn1"
    EXP_PARAMSET(2)%param(1)%table_info = "tb1"
    EXP_PARAMSET(2)%param(1)%stats_proc = "sp1"
    EXP_PARAMSET(2)%param(1)%fixed_sfc1_type = "fs1"
    allocate(EXP_PARAMSET(2)%param(1)%scale_fact_fixed_sfc1(1))
    EXP_PARAMSET(2)%param(1)%scale_fact_fixed_sfc1 = 0
    allocate(EXP_PARAMSET(2)%param(1)%level(1))
    EXP_PARAMSET(2)%param(1)%level = 0.0
    EXP_PARAMSET(2)%param(1)%fixed_sfc2_type = "fs2"
    allocate(EXP_PARAMSET(2)%param(1)%scale_fact_fixed_sfc2(1))
    EXP_PARAMSET(2)%param(1)%scale_fact_fixed_sfc2 = 0
    EXP_PARAMSET(2)%param(1)%aerosol_type = "at1"
    EXP_PARAMSET(2)%param(1)%prob_type = "pt1"
    EXP_PARAMSET(2)%param(1)%typ_intvl_size = "tis1"
    EXP_PARAMSET(2)%param(1)%scale_fact_1st_size = 1
    EXP_PARAMSET(2)%param(1)%scale_val_1st_size = 1.0
    EXP_PARAMSET(2)%param(1)%scale_fact_2nd_size = 2
    EXP_PARAMSET(2)%param(1)%scale_val_2nd_size = 2.0
    EXP_PARAMSET(2)%param(1)%typ_intvl_wvlen = "tw1"
    EXP_PARAMSET(2)%param(1)%scale_fact_1st_wvlen = 3
    EXP_PARAMSET(2)%param(1)%scale_val_1st_wvlen = 3.0
    EXP_PARAMSET(2)%param(1)%scale_fact_2nd_wvlen = 4
    EXP_PARAMSET(2)%param(1)%scale_val_2nd_wvlen = 4.0
    EXP_PARAMSET(2)%param(1)%scale_fact_lower_limit = 5
    EXP_PARAMSET(2)%param(1)%scale_val_lower_limit = 5.0
    EXP_PARAMSET(2)%param(1)%scale_fact_upper_limit = 6
    EXP_PARAMSET(2)%param(1)%scale_val_upper_limit = 6.0
    allocate(EXP_PARAMSET(2)%param(1)%scale(1))
    EXP_PARAMSET(2)%param(1)%scale = 0.0
    EXP_PARAMSET(2)%param(1)%stat_miss_val = 7
    EXP_PARAMSET(2)%param(1)%leng_time_range_prev = 8
    EXP_PARAMSET(2)%param(1)%time_inc_betwn_succ_fld = 9
    EXP_PARAMSET(2)%param(1)%type_of_time_inc = "toi1"
    EXP_PARAMSET(2)%param(1)%stat_unit_time_key_succ = "su1"
    EXP_PARAMSET(2)%param(1)%bit_map_flag = "bm1"

    ! Test Case 3: gen_proc_type = 'ens_fcst'
    EXP_PARAMSET(3)%datset = "ps2"
    EXP_PARAMSET(3)%grid_num = 3
    EXP_PARAMSET(3)%sub_center = "sc2"
    EXP_PARAMSET(3)%version_no = "v2"
    EXP_PARAMSET(3)%local_table_vers_no = "lt2"
    EXP_PARAMSET(3)%sigreftime = "sr2"
    EXP_PARAMSET(3)%prod_status = "ps2"
    EXP_PARAMSET(3)%data_type = "dt2"
    EXP_PARAMSET(3)%gen_proc_type = "ens_fcst"
    EXP_PARAMSET(3)%time_range_unit = "hour"
    EXP_PARAMSET(3)%orig_center = "oc2"
    EXP_PARAMSET(3)%gen_proc = "gp2"
    EXP_PARAMSET(3)%packing_method = "pm2"
    EXP_PARAMSET(3)%field_datatype = "fd2"
    EXP_PARAMSET(3)%comprs_type = "ct2"
    EXP_PARAMSET(3)%type_ens_fcst = "ens1"
    allocate(EXP_PARAMSET(3)%param(1))
    EXP_PARAMSET(3)%param(1)%post_avblfldidx = 201
    EXP_PARAMSET(3)%param(1)%shortname = "sn2"
    EXP_PARAMSET(3)%param(1)%longname = "longname2"
    EXP_PARAMSET(3)%param(1)%pname = "pn2"
    EXP_PARAMSET(3)%param(1)%table_info = "tb2"
    EXP_PARAMSET(3)%param(1)%stats_proc = "sp2"
    EXP_PARAMSET(3)%param(1)%fixed_sfc1_type = "fs1p"
    allocate(EXP_PARAMSET(3)%param(1)%scale_fact_fixed_sfc1(1))
    EXP_PARAMSET(3)%param(1)%scale_fact_fixed_sfc1 = 7
    allocate(EXP_PARAMSET(3)%param(1)%level(1))
    EXP_PARAMSET(3)%param(1)%level = 1000.0
    EXP_PARAMSET(3)%param(1)%fixed_sfc2_type = "fs2p"
    allocate(EXP_PARAMSET(3)%param(1)%scale_fact_fixed_sfc2(1))
    EXP_PARAMSET(3)%param(1)%scale_fact_fixed_sfc2 = 8
    allocate(EXP_PARAMSET(3)%param(1)%level2(1))
    EXP_PARAMSET(3)%param(1)%level2 = 2000.0
    EXP_PARAMSET(3)%param(1)%aerosol_type = "at2"
    EXP_PARAMSET(3)%param(1)%prob_type = "pt2"
    EXP_PARAMSET(3)%param(1)%typ_intvl_size = "tis2"
    EXP_PARAMSET(3)%param(1)%scale_fact_1st_size = 1
    EXP_PARAMSET(3)%param(1)%scale_val_1st_size = 1.5
    EXP_PARAMSET(3)%param(1)%scale_fact_2nd_size = 2
    EXP_PARAMSET(3)%param(1)%scale_val_2nd_size = 2.5
    EXP_PARAMSET(3)%param(1)%typ_intvl_wvlen = "tw2"
    EXP_PARAMSET(3)%param(1)%scale_fact_1st_wvlen = 3
    EXP_PARAMSET(3)%param(1)%scale_val_1st_wvlen = 3.5
    EXP_PARAMSET(3)%param(1)%scale_fact_2nd_wvlen = 4
    EXP_PARAMSET(3)%param(1)%scale_val_2nd_wvlen = 4.5
    EXP_PARAMSET(3)%param(1)%scale_fact_lower_limit = 5
    EXP_PARAMSET(3)%param(1)%scale_val_lower_limit = 5.5
    EXP_PARAMSET(3)%param(1)%scale_fact_upper_limit = 6
    EXP_PARAMSET(3)%param(1)%scale_val_upper_limit = 6.5
    allocate(EXP_PARAMSET(3)%param(1)%scale(1))
    EXP_PARAMSET(3)%param(1)%scale = 9.0
    EXP_PARAMSET(3)%param(1)%stat_miss_val = 10
    EXP_PARAMSET(3)%param(1)%leng_time_range_prev = 11
    EXP_PARAMSET(3)%param(1)%time_inc_betwn_succ_fld = 12
    EXP_PARAMSET(3)%param(1)%type_of_time_inc = "toi2"
    EXP_PARAMSET(3)%param(1)%stat_unit_time_key_succ = "su2"
    EXP_PARAMSET(3)%param(1)%bit_map_flag = "bm2"

    tprec = 2.
    tclod = 1
    trdlw = 1.
    trdsw = 1.
    tsrfc = 1.
    tmaxmin = 1.
    td3d = 1.

    res = 0
    call read_postxconfig()

    if (num_post_afld .ne. EXP_NUM_POST_AFLD) then
        print *, "Test Failed for num_post_afld: Expected ", EXP_NUM_POST_AFLD, &
            " but got ", num_post_afld
        res = 1
    end if
    
    if (tprec .ne. EXP_TPREC .OR. tclod .ne. EXP_TPREC .OR. trdlw .ne. EXP_TPREC &
        .OR. trdsw .ne. EXP_TPREC .OR. tsrfc .ne. EXP_TPREC .OR. tmaxmin .ne. EXP_TPREC &
        .OR. td3d .ne. EXP_TPREC) then
        print *, "Test Failed for tprec, tclod, trdlw, trdsw, tsrfc, tmaxmin, or td3d."
        res = 1
    end if

    if (paramset_count .ne. EXP_PARAMSET_COUNT) then
        print *, "Test Failed for paramset_count: Expected ", EXP_PARAMSET_COUNT, &
            " but got ", paramset_count
        res = 1
    end if

    if (res .ne. 0) stop 30
    
    print *, "SUCCESS!"
end program test_xml_perl_data