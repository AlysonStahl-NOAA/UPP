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

    exp_pc = 0.0
    exp_kx = 34.2969360
    exp_lx = -43.6572571
    exp_tott = 60.0737305
    exp_prcpType = PRECIPS%CONVECTION

    exp_ept(1) = 3.78657196E+02
    exp_ept(2) = 3.40894562E+02
    exp_ept(3) = 3.22361389E+02
    exp_ept(4) = 3.12390656E+02
    exp_ept(5) = 3.07577484E+02
    exp_ept(6) = 3.06790283E+02
    exp_ept(7) = 3.10039032E+02
    exp_ept(8) = 3.18258545E+02
    exp_ept(9) = 3.33539337E+02
    exp_ept(10) = 3.59922333E+02

    exp_wbt(1) = 2.19768265E+02
    exp_wbt(2) = 2.28567307E+02
    exp_wbt(3) = 2.37400467E+02
    exp_wbt(4) = 2.46090515E+02
    exp_wbt(5) = 2.54864166E+02
    exp_wbt(6) = 2.63451843E+02
    exp_wbt(7) = 2.72300842E+02
    exp_wbt(8) = 2.81353973E+02
    exp_wbt(9) = 2.90605042E+02
    exp_wbt(10) = 3.00208923E+02

    exp_twp = 0.0
    exp_twp(3:5) = 6.82362318E+00

    res = 0
    call derive_fields(imp_physics,t, rh, pres, hgt, totalWater, totalCond,&
                           nz, topoK, hprcp, hcprcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)

    call check_expected_values(pc, kx, lx, tott, prcpType, ept, wbt, twp, &
                               exp_pc, exp_kx, exp_lx, exp_tott, exp_prcpType, &
                                exp_ept, exp_wbt, exp_twp, 1, res)

    if (res .ne. 0) stop 10

    ! Test Case 2: Precipitation threshold fails (Precipitation type is None)
    imp_physics = 11
    hcprcp = 0.0
    exp_prcpType = PRECIPS%NONE

    res = 0
    call derive_fields(imp_physics,t, rh, pres, hgt, totalWater, totalCond,&
                           nz, topoK, hprcp, hcprcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)

    call check_expected_values(pc, kx, lx, tott, prcpType, ept, wbt, twp, &
                               exp_pc, exp_kx, exp_lx, exp_tott, exp_prcpType, &
                                exp_ept, exp_wbt, exp_twp, 1, res)

    if (res .ne. 0) stop 20

    ! Test Case 3: Eligible cloud not found (Precipitation type is None)
    ! TODO: Replace the ??? with code that sets up the input values for the case where 
    ! the precipitation type should be None due to lack of eligible clouds. Where possible,
    ! reuse values from Test Case 2. if values are being reused, do not add redundant assignments.
    totalCond = 0.0
    totalCond(8:10) = 0.004

    res = 0
    call derive_fields(imp_physics,t, rh, pres, hgt, totalWater, totalCond,&
                           nz, topoK, hprcp, hcprcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)

    print *, "pc = ", pc
    print *, "kx = ", kx
    print *, "lx = ", lx
    print *, "tott = ", tott
    print *, "prcpType = ", prcpType
    do i = 1, nz
        print *, 'ept(', i, ') = ', ept(i)
    end do
    do i = 1, nz
        print *, 'wbt(', i, ') = ', wbt(i)
    end do
    do i = 1, nz
        print *, 'twp(', i, ') = ', twp(i)
    end do

    print *, "SUCCESS!"
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

    contains

    subroutine check_expected_values(pc, kx, lx, tott, prcpType, ept, wbt, twp, &
                                        exp_pc, exp_kx, exp_lx, exp_tott, exp_prcpType, &
                                        exp_ept, exp_wbt, exp_twp, test_case, res)
        implicit none
        real, intent(in) :: pc, kx, lx, tott
        integer, intent(in) :: prcpType
        real, intent(in) :: ept(nz), wbt(nz), twp(:)
        real, intent(in) :: exp_pc, exp_kx, exp_lx, exp_tott
        integer, intent(in) :: exp_prcpType
        real, intent(in) :: exp_ept(nz), exp_wbt(nz), exp_twp(:)
        integer, intent(in) :: test_case
        integer, intent(inout) :: res
        integer :: i

    if (prcpType .ne. exp_prcpType) then
        print *, "Test Case ", test_case, " returned wrong precipitation type: ", prcpType
        res = 1
    end if

    if (abs(pc - exp_pc) > tol) then
        print *, "Test Case ", test_case, " expected pc = ", exp_pc, ", but got ", pc
        res = 1
    end if
    if (abs(kx - exp_kx) > tol) then
        print *, "Test Case ", test_case, " expected kx = ", exp_kx, ", but got ", kx
        res = 1
    end if
    if (abs(lx - exp_lx) > tol) then
        print *, "Test Case ", test_case, " expected lx = ", exp_lx, ", but got ", lx
        res = 1
    end if
    if (abs(tott - exp_tott) > tol) then
        print *, "Test Case ", test_case, " expected tott = ", exp_tott, ", but got ", tott
        res = 1
    end if

    do i = 1, nz
        if (abs(ept(i) - exp_ept(i)) > tol) then
            print *, "Test Case ", test_case, " expected ept(", i, ") = ", exp_ept(i), ", but got ", ept(i)
            res = 1
        end if
        if (abs(wbt(i) - exp_wbt(i)) > tol) then
            print *, "Test Case ", test_case, " expected wbt(", i, ") = ", exp_wbt(i), ", but got ", wbt(i)
            res = 1
        end if
        if (abs(twp(i) - exp_twp(i)) > tol) then
            print *, "Test Case ", test_case, " expected twp(", i, ") = ", exp_twp(i), ", but got ", twp(i)
            res = 1
        end if
    end do
    end subroutine check_expected_values

end program test_derived_fields