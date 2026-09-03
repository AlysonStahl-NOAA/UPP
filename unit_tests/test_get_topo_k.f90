! This is a test program for UPP.
!
! This program tests the getTopoK() function from GFIP3.f
!
! Alyson Stahl, 9/2026
program test_get_topo_k
    implicit none
    integer, parameter :: nz = 10
    !
    integer :: i, res
    integer :: topo_k, exp_topo_k
    real :: alt, hgt(nz)

    interface
        function getTopoK(hgt, alt, nz)
            real, intent(in) :: hgt(nz)
            real, intent(in) :: alt
            integer, intent(in) :: nz
            integer :: getTopoK
        end function getTopoK
    end interface

    ! TODO: Replace the ??? below with code that initializes input values
    ! for a standard call of getTopoK(). These values will be used to test the "happy path"
    ! of the function and should cover as many branches as possible.
    ! Try to keep inputs physically realistic. You may modify the value of nz if needed.
    alt = 800.0
    hgt = (/ 12000.0, 10000.0, 8500.0, 7000.0, 5600.0, 4300.0, 3100.0, 2000.0, 1200.0, 100.0 /)

    exp_topo_k = 9
    res = 0

    topo_k = getTopoK(hgt, alt, nz)

    if (topo_k .ne. exp_topo_k) stop 10
    
    print *, "Success!"

end program test_get_topo_k