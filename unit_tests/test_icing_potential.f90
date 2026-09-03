! This is a test program for UPP.
!
! This program tests routines from the IcingPotential module.
!
! Alyson Stahl, 9/2026
program test_icing_potential
    use IcingPotential, only : icing_pot
    use DerivedFields, only : PRECIPS
    use CloudLayers, only : clouds_t
    implicit none

    real, parameter :: tol = 1.0e-6
    integer :: res

    res = 0

    ! Test Case 1: Valid & epresentative multilayer cloud column with mixed 
    ! thermodynamic conditions favorable for icing.
    call test_mixed_phase_cloud_column(res)
    if (res .ne. 0) stop 10

    print *, "Success!"

contains
    subroutine test_mixed_phase_cloud_column(res)
        integer, intent(inout) :: res
        integer, parameter :: nz = 8
        integer :: i
        real :: hgt(nz), rh(nz), t(nz), liqCond(nz), vv(nz)
        type(clouds_t) :: clouds
        !
        real :: ice_pot(nz), exp_ice_pot(nz)

        clouds%nLayers = 2
        clouds%wmnIdx = -1
        clouds%avv = 0.0
        clouds%topIdx = 0
        clouds%baseIdx = 0
        clouds%ctt = 0.0

        clouds%topIdx(1) = 1
        clouds%baseIdx(1) = 3
        clouds%ctt(1) = 250.0

        clouds%topIdx(2) = 5
        clouds%baseIdx(2) = 8
        clouds%ctt(2) = 265.0

        hgt = (/ 9000.0, 7800.0, 6600.0, 5400.0, 4200.0, 3000.0, 1800.0, 500.0 /)
        rh = (/ 98.0, 80.0, 96.0, 45.0, 90.0, 97.0, 60.0, 100.0 /)
        t = (/ 245.0, 255.0, 266.0, 275.0, 260.0, 265.0, 270.0, 274.0 /)
        liqCond = (/ 0.50, 0.10, 0.00, 0.00, 0.30, 0.15, 0.0005, 0.25 /)
        vv = (/ -0.60, -0.20, 0.10, 0.00, -0.40, -0.55, -0.10, 0.20 /)

        exp_ice_pot = (/ 0.0, 0.14929457, 0.60332412, 0.0, 0.79784530, 1.0, 0.05740103, 0.0 /)

        call icing_pot(hgt, rh, t, liqCond, vv, nz, clouds, ice_pot)

        do i = 1, nz
            print '(A,I0,A,F16.8)', 'ice_pot(', i, '): ', ice_pot(i)
        end do
    end subroutine test_mixed_phase_cloud_column
end program test_icing_potential