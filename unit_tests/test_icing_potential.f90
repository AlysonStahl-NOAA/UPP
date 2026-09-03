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

    call test_case2(res)
    if (res .ne. 0) stop 20
    
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

        exp_ice_pot = 0.0
        exp_ice_pot(1) = 0.0
        exp_ice_pot(2) = 0.149695575
        exp_ice_pot(3) = 0.603878081
        exp_ice_pot(4) = 0.0
        exp_ice_pot(5) = 0.797262013
        exp_ice_pot(6) = 1.0
        exp_ice_pot(7) = 0.057372101
        exp_ice_pot(8) = 0.0

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

        call icing_pot(hgt, rh, t, liqCond, vv, nz, clouds, ice_pot)

        do i = 1, nz
            if (abs(ice_pot(i) - exp_ice_pot(i)) > tol) then
                print *, "Expected ice_pot(", i, "): ", exp_ice_pot(i), & 
                        " but got: ", ice_pot(i)
            end if
        end do
    end subroutine test_mixed_phase_cloud_column


    subroutine test_case2(res)
        integer, intent(inout) :: res
        ! TODO: Replace the ??? below with a value for nz that is appropriate for the 
        ! set up of input/output variables below
        integer, parameter :: nz = 5
        integer :: i
        real :: hgt(nz), rh(nz), t(nz), liqCond(nz), vv(nz)
        type(clouds_t) :: clouds
        !
        real :: ice_pot(nz), exp_ice_pot(nz)

        ! TODO: Replace the ??? below with code that initializes all of the necessary input/output
        ! variables for a test of icing_pot() subroutine.
        ! The values you choose will be used for a unit test of the subroutine and any functions/
        ! subroutines that it calls. The values and the array sizes should result in a call where the ELSE
        ! branch executes for at least one call of rh_map
        !
        ! if (rh>95.0) then
        !   rh_map=1.0
        ! elseif ( (rh<=95.0).and.(rh>=50.0) ) then 
        !    rh_map=((20./9.) * ((rh/100.0)-0.5))**3.0
        ! else
        !    rh_map=0.0
        ! endif
        !
        ! Meaning there should be at least one value of rh < 50.0 so that rh_map=0.0 for that value.
        ! Pay close attention to the way that any array values should vary with height and what height an
        ! index should represent. Also pay attention to the code surrounding this if statement so you're not missing
        ! any other conditions that might affect the outcomes of the if statement.
        exp_ice_pot = 0.0
        exp_ice_pot(1) = 1.0
        exp_ice_pot(2) = 0.0
        exp_ice_pot(3) = 0.761865556
        exp_ice_pot(4) = 0.384259254
        exp_ice_pot(5) = 0.0

        clouds%nLayers = 1
        clouds%wmnIdx = -1
        clouds%avv = 0.0
        clouds%topIdx = 0
        clouds%baseIdx = 0
        clouds%ctt = 0.0

        clouds%topIdx(1) = 1
        clouds%baseIdx(1) = 5
        clouds%ctt(1) = 265.0

        hgt = (/ 7000.0, 5500.0, 4000.0, 2500.0, 1000.0 /)
        rh = (/ 100.0, 45.0, 90.0, 80.0, 98.0 /)
        t = (/ 263.15, 260.15, 265.0, 263.15, 274.0 /)
        liqCond = (/ 0.30, 0.20, 0.10, 0.0005, 0.25 /)
        vv = (/ -0.60, -0.30, 0.20, -0.25, -0.40 /)

        call icing_pot(hgt, rh, t, liqCond, vv, nz, clouds, ice_pot)

        do i = 1, nz
            print '(A,I0,A,ES24.16)', 'ice_pot(', i, '): ', ice_pot(i)
        end do

        ! do i = 1, nz
        !     if (abs(ice_pot(i) - exp_ice_pot(i)) > tol) then
        !         print *, "Expected ice_pot(", i, "): ", exp_ice_pot(i), & 
        !                 " but got: ", ice_pot(i)
        !     end if
        ! end do
    end subroutine test_case2

end program test_icing_potential