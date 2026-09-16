! This is a test program for UPP.
!
! This program tests the icing_algo() subroutine.
!
! Alyson Stahl, 9/2026
program test_icing_algo
    use ctlblk_mod, only: imp_physics, spval, DTQ2, me
    use CloudLayers,    only : clouds_t
    implicit none

    real, parameter :: tol = 1.0e-6
    integer :: res

    res = 0

    DTQ2 = 3600.0
    spval = 9.9e10

    call test_imp_physics_98(res)
    if (res .ne. 0) stop 10

    call test_imp_physics_11(res)
    if (res .ne. 0) stop 20

    print *, "Success!"
contains

    subroutine test_imp_physics_98(res)
        integer, intent(inout) :: res
        integer, parameter :: nz = 5
        integer :: i, j, k
        real :: pres(nz), temp(nz), rh(nz), hgt(nz), omega(nz), wh(nz)
        real :: q(nz), cwat(nz), qqw(nz), qqi(nz), qqr(nz), qqs(nz), qqg(nz)
        real :: xlat, xlon, xalt, prate, cprate, cape, cin
        real :: ice_pot(nz), ice_sev(nz), expected_ice_pot(nz), expected_ice_sev(nz)

        imp_physics = 98
        i = 10
        j = 20

        pres = (/ 30000.0, 45000.0, 60000.0, 75000.0, 90000.0 /)
        temp = (/ 248.15, 255.15, 259.15, 264.15, 270.15 /)
        rh = (/ 60.0, 60.0, 60.0, 60.0, 60.0 /)
        hgt = (/ 9000.0, 7000.0, 5000.0, 2500.0, 0.0 /)
        omega = (/ spval, -0.12, -0.10, -0.08, -0.05 /)
        wh = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        q = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        cwat = (/ spval, 1.0e-4, 1.5e-4, 2.0e-4, 1.0e-4 /)
        qqw = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        qqi = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        qqr = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        qqs = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        qqg = (/ spval, 0.0, 0.0, 0.0, 0.0 /)

        xlat = 45.0
        xlon = -97.0
        xalt = 0.0
        prate = 0.0
        cprate = 0.0
        cape = 100.0
        cin = -200.0

        expected_ice_pot = 0.0
        expected_ice_sev = 0.0

        call icing_algo(i, j, pres, temp, rh, hgt, omega, wh, q, cwat, qqw, qqi, qqr, qqs, qqg, &
             nz, xlat, xlon, xalt, prate, cprate, cape, cin, ice_pot, ice_sev)

        do k = 1, nz
            if (abs(ice_pot(k) - expected_ice_pot(k)) > tol) then
                print *, "icing_algo() imp_physics=98 ice_pot failed at index ", k, ": expected ", &
                      expected_ice_pot(k), " but got ", ice_pot(k)
                res = 1
            end if
            if (abs(ice_sev(k) - expected_ice_sev(k)) > tol) then
                print *, "icing_algo() imp_physics=98 ice_sev failed at index ", k, ": expected ", &
                      expected_ice_sev(k), " but got ", ice_sev(k)
                res = 1
            end if
        end do
    end subroutine test_imp_physics_98

    subroutine test_imp_physics_11(res)
        integer, intent(inout) :: res
        integer, parameter :: nz = 5
        integer :: i, j, k
        real :: pres(nz), temp(nz), rh(nz), hgt(nz), omega(nz), wh(nz)
        real :: q(nz), cwat(nz), qqw(nz), qqi(nz), qqr(nz), qqs(nz), qqg(nz)
        real :: xlat, xlon, xalt, prate, cprate, cape, cin
        real :: ice_pot(nz), ice_sev(nz), expected_ice_pot(nz), expected_ice_sev(nz)

        imp_physics = 11
        i = 30
        j = 40

        pres = (/ 30000.0, 45000.0, 60000.0, 75000.0, 90000.0 /)
        temp = (/ 248.15, 255.15, 260.15, 265.15, 270.15 /)
        rh = (/ 60.0, 60.0, 60.0, 60.0, 60.0 /)
        hgt = (/ 9000.0, 7000.0, 5000.0, 2500.0, 0.0 /)
        omega = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        wh = (/ spval, -0.20, -0.12, -0.08, -0.04 /)
        q = (/ spval, 1.0e-3, 1.5e-3, 2.0e-3, 2.5e-3 /)
        cwat = (/ spval, 0.0, 0.0, 0.0, 0.0 /)
        qqw = (/ spval, 5.0e-5, 8.0e-5, 1.0e-4, 6.0e-5 /)
        qqi = (/ spval, 4.0e-5, 3.0e-5, 2.0e-5, 1.0e-5 /)
        qqr = (/ spval, 2.0e-5, 3.0e-5, 4.0e-5, 2.0e-5 /)
        qqs = (/ spval, 1.0e-5, 2.0e-5, 1.0e-5, 2.0e-5 /)
        qqg = (/ spval, 0.0, 1.0e-5, 2.0e-5, 0.0 /)

        xlat = 45.0
        xlon = -97.0
        xalt = 0.0
        prate = 0.0
        cprate = 0.0
        cape = 100.0
        cin = -200.0

        expected_ice_pot = 0.0
        expected_ice_sev = 0.0

        call icing_algo(i, j, pres, temp, rh, hgt, omega, wh, q, cwat, qqw, qqi, qqr, qqs, qqg, &
             nz, xlat, xlon, xalt, prate, cprate, cape, cin, ice_pot, ice_sev)

        do k = 1, nz
            if (abs(ice_pot(k) - expected_ice_pot(k)) > tol) then
                print *, "icing_algo() imp_physics=11 ice_pot failed at index ", k, ": expected ", &
                      expected_ice_pot(k), " but got ", ice_pot(k)
                res = 1
            end if
            if (abs(ice_sev(k) - expected_ice_sev(k)) > tol) then
                print *, "icing_algo() imp_physics=11 ice_sev failed at index ", k, ": expected ", &
                      expected_ice_sev(k), " but got ", ice_sev(k)
                res = 1
            end if
        end do
    end subroutine test_imp_physics_11



end program test_icing_algo