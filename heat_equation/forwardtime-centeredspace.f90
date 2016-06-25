program ftcs
implicit none

real, allocatable, dimension(:) :: temp(:,:)
real :: k, l
real :: t, tau, x, pos
integer(8) :: i, n, j, h

i = 1
j = 1

k = 1.0
l = 1.0
h = 61
n = 500
tau = 0.0001
x = l / (h - 1)

allocate(temp(n, h))

!temp(time, space)
temp(:,1) = 0.0
temp(:,h) = 0.0

temp(1,:) = 0.0
temp(1,int(h/2)) = 1.0 / x

open(unit = 15, file="temp.dat")

do i = 1, n
    do j = 2, h - 1
        temp(i + 1,j) = ((k * tau) / x**2) * (temp(i,j+1) + temp(i,j-1) - 2 * temp(i,j)) + temp(i,j)
         pos = pos + x
    enddo
    temp(i + 1, 1) = 0.0
    temp(i + 1, h) = 0.0
    t = t + tau
enddo

write(*, *) temp(1,int(h/2.0))
do i = 1, n, 5
    write(15,*) temp(i,:)
enddo

end

