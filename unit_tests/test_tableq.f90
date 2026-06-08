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
    character(len=*), parameter :: data_file_name = 'data/ref_tableq.txt'
    !
    real :: PL, THL
    real :: TTBLQ(JTB,ITB)
    real :: STHE(ITB), THE0(ITB)
    real :: RDP, RDTHE
    !
    real :: EXP_TTBLQ(JTB,ITB), EXP_STHE(ITB), EXP_THE0(ITB)
    real :: EXP_RDP, EXP_RDTHE

    ! RDP = 1./DP -> DP = (PH - PL) / REAL(KPM - 1) = (PH - PL) / REAL(ITB - 1) = (105000.- PL) / 75
    ! RDTHE = 1./DTHE -> DTHE = 1 / REAL(KTHM - 1) = 1 / (JTB - 1) = 1 / 133

    ! Same values used in INITPOST_GFS_NEMS_MPIIO()
    PL = 70000.0
    THL = 210.0

    call TABLEQ(TTBLQ, RDP, RDTHE, PL, THL, STHE, THE0)

    call write_tableq_reference_data(data_file_name, PL, THL, TTBLQ, STHE, THE0)
    print *, 'SUCCESS!'

contains

    subroutine write_tableq_reference_data(filename, pl_in, thl_in, ttblq_in, sthe_in, the0_in)
        character(len=*), intent(in) :: filename
        real, intent(in) :: pl_in, thl_in
        real, intent(in) :: ttblq_in(JTB,ITB), sthe_in(ITB), the0_in(ITB)
        integer :: unit_num, i, j
        
        open(newunit=unit_num, file=filename, status='replace', action='write')
        
        write(unit_num, '(a)') '# Reference data for TABLEQ() subroutine test'
        write(unit_num, '(a,f10.1,a,f10.1)') '# Test case with PL=', pl_in, ', THL=', thl_in
        write(unit_num, '(a,i0,a,i0,a,i0,a)') &
            '# Format: TTBLQ(', JTB*ITB, ' values), STHE(', ITB, '), THE0(', ITB, ')'
        
        do i = 1, ITB
            do j = 1, JTB
                write(unit_num, '(es24.16)') ttblq_in(j,i)
            end do
        end do
        
        do i = 1, ITB
            write(unit_num, '(es24.16)') sthe_in(i)
        end do
        
        do i = 1, ITB
            write(unit_num, '(es24.16)') the0_in(i)
        end do
        
        close(unit_num)
    end subroutine write_tableq_reference_data
    
end program test_tableq