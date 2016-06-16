program Array
Implicit none

Real xstep, x(10)
integer i

x(1) = 0.0
xstep = 6.0

do i=1,9
    x(i+1) = x(i) + xstep
enddo

do i=1,9
    write(*,*) 'x(', i, ') = ', x(i)
enddo

!allocate(xrad(n),y(n

end
