subroutine make_a(kmat_n, kmat, ped_n, ped, offset)
 implicit none
!define inputs
 integer, intent(in) :: kmat_n, ped_n, ped(3, ped_n), offset
 real, intent(inout) :: kmat(kmat_n, kmat_n)
!define variables derived in subroutine
 integer :: i, j
 real,dimension(kmat_n)::pvec1,pvec2,pvec3,pvec4,pvec5,pvec6,pvec7,pvec8,pvec9,pvec10
 real, dimension(kmat_n)::pvec11,pvec12,pvec13,pvec14,pvec15,pvec16,pvec17,pvec18,pvec19,pvec20
!now the program that computes the kinship coefficients

 do i=1, ped_n, 20
!1
    pvec1=(kmat(ped(2,i), :) + kmat(ped(3,i), :)) * 0.5
    kmat(:, i+offset)=pvec1 
    kmat(i+offset, :)=pvec1
    kmat(i+offset,i+offset)=(1 + kmat(ped(2,i), ped(3,i))) * 0.5
!2
    pvec2=(kmat(ped(2,i+1), :) + kmat(ped(3,i+1), :)) * 0.5 
    kmat(:, i+1+offset)=pvec2
    kmat(i+1+offset, :)=pvec2 
    kmat(i+1+offset,i+1+offset)=(1 + kmat(ped(2,i+1), ped(3,i+1))) * 0.5
!3
    pvec3=(kmat(ped(2,i+2), :) + kmat(ped(3,i+2), :)) * 0.5 
    kmat(:, i+2+offset)=pvec3
    kmat(i+2+offset, :)=pvec3
    kmat(i+2+offset,i+2+offset)=(1 + kmat(ped(2,i+2), ped(3,i+2))) * 0.5
!4
    pvec4=(kmat(ped(2,i+3), :) + kmat(ped(3,i+3), :)) * 0.5
    kmat(:, i+3+offset)=pvec4
    kmat(i+3+offset, :)=pvec4 
    kmat(i+3+offset,i+3+offset)=(1 + kmat(ped(2,i+3), ped(3,i+3))) * 0.5
!5
    pvec5=(kmat(ped(2,i+4), :) + kmat(ped(3,i+4), :)) * 0.5
    kmat(:, i+4+offset)=pvec5 
    kmat(i+4+offset, :)=pvec5 
    kmat(i+4+offset,i+4+offset)=(1 + kmat(ped(2,i+4), ped(3,i+4))) * 0.5
!6
    pvec6=(kmat(ped(2,i+5), :) + kmat(ped(3,i+5), :)) * 0.5 
    kmat(:, i+5+offset)=pvec6
    kmat(i+5+offset, :)=pvec6 
    kmat(i+5+offset,i+5+offset)=(1 + kmat(ped(2,i+5), ped(3,i+5))) * 0.5
!7
    pvec7=(kmat(ped(2,i+6), :) + kmat(ped(3,i+6), :)) * 0.5 
    kmat(:, i+6+offset)=pvec7
    kmat(i+6+offset, :)=pvec7
    kmat(i+6+offset,i+6+offset)=(1 + kmat(ped(2,i+6), ped(3,i+6))) * 0.5
!8
    pvec8=(kmat(ped(2,i+7), :) + kmat(ped(3,i+7), :)) * 0.5 
    kmat(:, i+7+offset)=pvec8
    kmat(i+7+offset, :)=pvec8 
    kmat(i+7+offset,i+7+offset)=(1 + kmat(ped(2,i+7), ped(3,i+7))) * 0.5
!9
    pvec9=(kmat(ped(2,i+8), :) + kmat(ped(3,i+8), :)) * 0.5 
    kmat(:, i+8+offset)=pvec9
    kmat(i+8+offset, :)=pvec9 
    kmat(i+8+offset,i+8+offset)=(1 + kmat(ped(2,i+8), ped(3,i+8))) * 0.5
!10
    pvec10=(kmat(ped(2,i+9), :) + kmat(ped(3,i+9), :)) * 0.5 
    kmat(:, i+9+offset)=pvec10
    kmat(i+9+offset, :)=pvec10 
    kmat(i+9+offset,i+9+offset)=(1 + kmat(ped(2,i+9), ped(3,i+9))) * 0.5
!11
    pvec11=(kmat(ped(2,i+10), :) + kmat(ped(3,i+10), :)) * 0.5
    kmat(:, i+10+offset)=pvec11 
    kmat(i+10+offset, :)=pvec11 
    kmat(i+10+offset,i+10+offset)=(1 + kmat(ped(2,i+10), ped(3,i+10))) * 0.5
!12
    pvec12=(kmat(ped(2,i+11), :) + kmat(ped(3,i+11), :)) * 0.5
    kmat(:, i+11+offset)=pvec12 
    kmat(i+11+offset, :)=pvec12 
    kmat(i+11+offset,i+11+offset)=(1 + kmat(ped(2,i+11), ped(3,i+11))) * 0.5
!13
    pvec13=(kmat(ped(2,i+12), :) + kmat(ped(3,i+12), :)) * 0.5
    kmat(:, i+12+offset)=pvec13 
    kmat(i+12+offset, :)=pvec13 
    kmat(i+12+offset,i+12+offset)=(1 + kmat(ped(2,i+12), ped(3,i+12))) * 0.5
!14
    pvec14=(kmat(ped(2,i+13), :) + kmat(ped(3,i+13), :)) * 0.5
    kmat(:, i+13+offset)=pvec14 
    kmat(i+13+offset, :)=pvec14 
    kmat(i+13+offset,i+13+offset)=(1 + kmat(ped(2,i+13), ped(3,i+13))) * 0.5
!15
    pvec15=(kmat(ped(2,i+14), :) + kmat(ped(3,i+14), :)) * 0.5 
    kmat(:, i+14+offset)=pvec15
    kmat(i+14+offset, :)=pvec15
    kmat(i+14+offset,i+14+offset)=(1 + kmat(ped(2,i+14), ped(3,i+14))) * 0.5
!16
    pvec16=(kmat(ped(2,i+15), :) + kmat(ped(3,i+15), :)) * 0.5 
    kmat(:, i+15+offset)=pvec16
    kmat(i+15+offset, :)=pvec16 
    kmat(i+15+offset,i+15+offset)=(1 + kmat(ped(2,i+15), ped(3,i+15))) * 0.5
!17
    pvec17=(kmat(ped(2,i+16), :) + kmat(ped(3,i+16), :)) * 0.5 
    kmat(:, i+16+offset)=pvec17
    kmat(i+16+offset, :)=pvec17 
    kmat(i+16+offset,i+16+offset)=(1 + kmat(ped(2,i+16), ped(3,i+16))) * 0.5
!18
    pvec18=(kmat(ped(2,i+17), :) + kmat(ped(3,i+17), :)) * 0.5
    kmat(:, i+17+offset)=pvec18 
    kmat(i+17+offset, :)=pvec18 
    kmat(i+17+offset,i+17+offset)=(1 + kmat(ped(2,i+17), ped(3,i+17))) * 0.5
!19
    pvec19=(kmat(ped(2,i+18), :) + kmat(ped(3,i+18), :)) * 0.5 
    kmat(:, i+18+offset)=pvec19
    kmat(i+18+offset, :)=pvec19 
    kmat(i+18+offset,i+18+offset)=(1 + kmat(ped(2,i+18), ped(3,i+18))) * 0.5
!20
    pvec20=(kmat(ped(2,i+19), :) + kmat(ped(3,i+19), :)) * 0.5 
    kmat(:, i+19+offset)=pvec20
    kmat(i+19+offset, :)=pvec20 
    kmat(i+19+offset,i+19+offset)=(1 + kmat(ped(2,i+19), ped(3,i+19))) * 0.5
 end do
end subroutine make_a
