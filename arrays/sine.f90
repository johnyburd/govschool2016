program plotsinearray
implicit none

real xstep, xrad(501), y(501), pi
integer i, n

! plots sine from -2pi to 2pi

pi = atan(1.0) * 4
n = 500
xstep = (4 * pi) / float(n)

xrad(1) = -2.0*pi

do i = 1, n
    y(i) = sin(xrad(i))
    xrad(i+1) = xrad(i) + xstep
enddo

open(unit = 20, file='sine.dat')
write(20,*) "# sine garbage"

do i = 1, n
    write(20,*) xrad(i), y(i)
enddo

end
