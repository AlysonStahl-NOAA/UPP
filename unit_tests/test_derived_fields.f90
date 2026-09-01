! This is a test program for UPP.
!
! This program tests routines from the DerivedFields module.
!
! Alyson Stahl, 8/2026
program test_derived_fields
    use DerivedFields, only: derive_fields, PRECIPS
    implicit none

    real, parameter :: tol = 1.0e-6
    integer, parameter :: nz = 10
    integer :: i, j, k, res
    integer :: imp_physics, topoK
    real, dimension(nz) :: t, rh, pres, hgt, totalWater, totalCond
    real :: hprcp, hcprcp, cin, cape
    !
    real, dimension(nz) :: ept, wbt, twp
    real :: pc, kx, lx, tott
    integer :: prcpType
    !
    real, dimension(nz) :: exp_ept, exp_wbt, exp_twp
    real :: exp_pc, exp_kx, exp_lx, exp_tott
    integer :: exp_prcpType

    ! Test Case 1: Convection
    imp_physics = 99
    topoK = nz

    do k = 1, nz
        pres(k) = 5000.0 + 10000.0 * real(k)
        hgt(k) = 17000.0 - 1800.0 * real(k - 1)
        t(k) = 220.0 + 9.0 * real(k - 1)
        rh(k) = 40.0 + 6.0 * real(k - 1)
    end do

    totalWater = 0.0
    totalWater(3:5) = 0.002
    totalWater(8:10) = 0.002
    totalCond = 0.0

    hprcp = 0.10
    hcprcp = 1.50
    cin = -50.0
    cape = 1800.0

    res = 0
    call derive_fields(imp_physics,t, rh, pres, hgt, totalWater, totalCond,&
                           nz, topoK, hprcp, hcprcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)

    if (prcpType .ne. PRECIPS%CONVECTION) then
        print *, "Test Case 1 returned wrong precipitation type: ", prcpType
        res = 1
    end if

    print *, "pc: ", pc
    print *, "kx: ", kx
    print *, "lx: ", lx
    print *, "tott: ", tott
    
    do i = 1, nz
        print '(A,I0,A,ES16.8)', "ept(", i, "): ", ept(i)
    end do
    
    do i = 1, nz
        print '(A,I0,A,ES16.8)', "wbt(", i, "): ", wbt(i)
    end do

    do i = 1, nz
        print '(A,I0,A,ES16.8)', "twp(", i, "): ", twp(i)
    end do

    print *, "End of Test Case 1"
    ! Test Case 2: Precipitation threshold fails (Precipitation type is None)
    imp_physics = 11
    hcprcp = 0.0

    do k = 1, nz
        pres(k) = 5000.0 + 10000.0 * real(k)
    end do

    pres(5) = 50000.0
    pres(7) = 70000.0
    pres(8) = 80000.0
    pres(9) = 90000.0

    res = 0
    call derive_fields(imp_physics,t, rh, pres, hgt, totalWater, totalCond,&
                           nz, topoK, hprcp, hcprcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)

    if (prcpType .ne. PRECIPS%NONE) then
        print *, "Test Case 2 returned wrong precipitation type: ", prcpType
        res = 1
    end if

    print *, "pc: ", pc
    print *, "kx: ", kx
    print *, "lx: ", lx
    print *, "tott: ", tott
    
    do i = 1, nz
        print '(A,I0,A,ES16.8)', "ept(", i, "): ", ept(i)
    end do
    
    do i = 1, nz
        print '(A,I0,A,ES16.8)', "wbt(", i, "): ", wbt(i)
    end do

    do i = 1, nz
        print '(A,I0,A,ES16.8)', "twp(", i, "): ", twp(i)
    end do
    print *, "End of Test Case 2"

    ! getThetaw => gives  wbt
    !   i) dry air: td << t, rh << 100%, lower pres
    !   ii) moist air: td ~ t, rh ~ 100%, higher pres

    ! calc_totalWaterPath => gives twp
    !   i) totalWater(k) <= 0.001, topIdx == -1, baseIdx == -1, twp(k) = 0.0
    !   ii) totalWater(k) > 0.001, all layer temps > 273.15, twp(k) = 0.0
    !   iii) totalWater(k) > 0.001, at least one detected layer level has t <= 273.15, twp(k) > 0.0

    ! WILL REQUIRE AT LEAST 3 CALLS
    ! calc_indice => gives kIndex (kx), liftedIndex (lx), totalTotals (tott) 
    !   i) pres(k) >= 50000.0 & pres(k-1) < 50000.0
    !       a) abs(pres(k) - 50000.0) <= 0.1
    !       b) abs(pres(k-1)- 50000.0) <= 0.1
    !       c)  not a or b
    !   ii) (pres(k)- 70000.0 >= 0.) .and. (pres(k-1)- 70000.0 < 0.)
    !       a) abs(pres(k) - 70000.0) <= 0.1
    !       b) abs(pres(k-1)- 70000.0) <= 0.1
    !       c)  not a or b
    !   iii) ((pres(k)- 85000.0 >= 0.) .and. (pres(k-1)- 85000.0 < 0.)
    !       a) abs(pres(k) - 85000.0) <= 0.1
    !       b) abs(pres(k-1)- 85000.0) <= 0.1
    !       c)  not a or b

    ! getPrecipType =>  gives prcpType
    ! Executes as hierarchy. If one condition is met, the subsequent conditions are not evaluated.
    !   i) Convection: all are true for hcprcp >= 1.0, cape >= 1000.0, cin > -100.0, lx < 0.0
    !   ii) None: Convection fails and:
    !       a) Precipitation threshold fails
    !           aa) imp_physics = 98/99 & hprcp < 0.045
    !           ab) imp_physics = 11/8 & pc < 0.01
    !       b) Eligible Cloud not found 
    !           ba) At relevant final level(s) pres(k) < 15000.0
    !           bb) At relevant final level(s) wbt(k) < 200.0
    !           bc) At relevant final level(s) wbt(k) > 1000.0
    !           bd) At relevant final level(s) cloud search never changes iceIdx (so iceIdx == k)
    !   iii) Snow: Not convection or None, coldTemp <= 265.15, tColdArea < 350.0, & t(k) <= 273.15
    !   iv) Rain: Not convection or None, coldTemp <= 265.15 and t(k) > 273.15
    !       a) tColdArea < 350.0
    !       b) tColdArea >= 350.0 & wetBuldArea > -250.0
    !   v) Other: Not convection or None and one of the following conditions is met:
    !       a) coldTemp > 265.15
    !       b) tcoldTemp <= 265.15 & tColdArea >= 350.0 & wetBuldArea <= -250.0
    !       c) coldTemp <= 265.15 & tColdArea >= 350.0 & wetBuldArea > -250.0 & t(k) > 273.15

end program test_derived_fields