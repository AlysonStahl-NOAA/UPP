! This is a test program for UPP.
!
! This program tests the LFMFLD_GFS() subroutine.
!
! Alyson Stahl, 5/2026
program test_lfmfld_gfs
    use vrbls3d, only: pint, q, t, pmid
    use masks, only: lmh
    use params_mod, only: d00
    use ctlblk_mod, only: jsta, jend, modelname, spval, ista, iend
    implicit none

    real, parameter :: tol = 1.0e-8
    integer, parameter :: npts = 6, nlevs = 30
    integer :: i, j, res
    integer :: lm
    real :: RH4410(1, npts), RH7294(1, npts), RH4472(1, npts), RH3310(1, npts)
    real :: EXP_RH4410(1, npts), EXP_RH7294(1, npts), EXP_RH4472(1, npts), EXP_RH3310(1, npts)

    interface
        subroutine LFMFLD_GFS(RH4410,RH7294,RH4472,RH3310)
            use ctlblk_mod, only: jsta, jend, ista, iend
            real, dimension(ista:iend,jsta:jend), intent(out) :: RH4410, RH7294, RH4472, RH3310
        end subroutine LFMFLD_GFS
    end interface

    ! Grid Dimensions
    ista = 1
    iend = 1
    jsta = 1
    jend = npts
    lm = nlevs
    spval = 9.9e10

    allocate(pint(ista:iend, jsta:jend, 1:lm+1))
    allocate(t(ista:iend, jsta:jend, 1:lm))
    allocate(q(ista:iend, jsta:jend, 1:lm))
    allocate(pmid(ista:iend, jsta:jend, 1:lm))
    allocate(lmh(ista:iend, jsta:jend))

    ! Test Case 1: Default case where none of the RH values are spval.
    lmh = real(lm)
    do i = 1, lm + 1
        pint(1, :, i)   = 5000.0 + real(i-1) / real(lm) * 95000.0
    end do
    do i = 1, lm
        pmid(1, :, i) = 0.5 * (pint(1, :, i) + pint(1, :, i+1))
        t(1, :, i)    = 288.0 - 6.5e-3 * real(i-1) / real(lm) * 11000.0
        q(1, :, i)    = 0.01 * exp(-real(i-1) / real(lm) * 3.0)
    end do

    ! TODO: Replace with the actual expected value calculations once I have them.
    EXP_RH4410 = 0.0
    EXP_RH7294 = 0.0
    EXP_RH4472 = 0.0
    EXP_RH3310 = 0.0

    ! Test Case 2: RH4472=spval
    lmh(1, 2) = 5.0
    do i = 1, 6
        pint(1, 2, i) = 72000.0 + real(i-1) / 5.0 * 28000.0
    end do
    EXP_RH4410(1, 2) = 0.0
    EXP_RH7294(1, 2) = 0.0
    EXP_RH4472(1, 2) = spval
    EXP_RH3310(1, 2) = 0.0

    ! Test Case 3: RH7294=spval
    lmh(1, 3) = 6.0
    do i = 1, 7
        pint(1, 3, i) = 50000.0 + real(i-1) / 6.0 * 22000.0
    end do
    EXP_RH4410(1, 3) = 0.0
    EXP_RH7294(1, 3) = spval
    EXP_RH4472(1, 3) = 0.0
    EXP_RH3310(1, 3) = 0.0
    
    ! Test Case 4: RH4472=spval AND RH7294=spval
    lmh(1, 4) = 3.0
    do i = 1, 4
        pint(1, 4, i) = 94000.0 + real(i-1) / 3.0 * 6000.0
    end do
    EXP_RH4410(1, 4) = 0.0
    EXP_RH7294(1, 4) = spval
    EXP_RH4472(1, 4) = spval
    EXP_RH3310(1, 4) = 0.0

    ! Test Case 5: Only RH3310!=spval
    lmh(1, 5) = 3.0
    do i = 1, 4
        pint(1, 5, i) = 33000.0 + real(i-1) / 3.0 * 11000.0
    end do
    EXP_RH4410(1, 5) = spval
    EXP_RH7294(1, 5) = spval
    EXP_RH4472(1, 5) = spval
    EXP_RH3310(1, 5) = 0.0

    ! Test Case 6: All spval 
    lmh(1, 6) = 1.0
    pint(1, 6, 1) = 10000.0
    pint(1, 6, 2) = 20000.0
    EXP_RH4410(1, 6) = spval
    EXP_RH7294(1, 6) = spval
    EXP_RH4472(1, 6) = spval
    EXP_RH3310(1, 6) = spval

    res = 0
    call LFMFLD_GFS(RH4410, RH7294, RH4472, RH3310)

    do i = 1, npts
        print '(A, I0, A, ES16.9)', "RH4410(", i, ") = ", RH4410(1, i)
        print '(A, I0, A, ES16.9)', "RH7294(", i, ") = ", RH7294(1, i)
        print '(A, I0, A, ES16.9)', "RH4472(", i, ") = ", RH4472(1, i)
        print '(A, I0, A, ES16.9)', "RH3310(", i, ") = ", RH3310(1, i)
    end do

    deallocate(pint, t, q, pmid, lmh)

    if (res .ne. 0) stop 10

    print *, "SUCCESS!"
end program test_lfmfld_gfs