! This is a test program for UPP.
!
! This program tests the TABLEQ() subroutine.
!
! Alyson Stahl, 6/2026
program test_tableq
    use tableq_upp_mod, only: TABLEQ
    implicit none

    real, parameter :: tol = 1.0e-8
    integer, parameter :: ITB=152, JTB=440
    integer :: i, j, res
    !
    real :: PL, THL
    real :: TTBLQ(JTB,ITB)
    real :: STHE(ITB), THE0(ITB)
    real :: RDP, RDTHE
    !
    real :: EXP_RDP, EXP_RDTHE

    ! RDP = 1./DP -> DP = (PH - PL) / REAL(KPM - 1) = (PH - PL) / REAL(ITB - 1) = (105000.- PL) / 75
    ! RDTHE = 1./DTHE -> DTHE = 1 / REAL(KTHM - 1) = 1 / (JTB - 1) = 1 / 133

    ! Same values used in INITPOST_GFS_NEMS_MPIIO()
    PL = 70000.0
    THL = 210.0

    call TABLEQ(TTBLQ, RDP, RDTHE, PL, THL, STHE, THE0)


    print *, 'SUCCESS!'
end program test_tableq