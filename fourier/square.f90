program square

implicit none
real xstep, pi, x, two, three, four, alot, rem
integer i, n, j

alot = 0.5
pi = atan(1.0) * 4
n = 500
xstep = (4 * pi) / float(n)
x = -2.0 * pi

open(unit = 20, file='twoterm.dat')
open(unit = 21, file='threeterm.dat')
open(unit = 22, file='fourterm.dat')
open(unit = 23, file='500term.dat')

do i = 1, n
    alot = 0.5
    two = 0.5 + (2.0/pi)*sin(x)
    three = 0.5 + (2.0/pi)*sin(x) + ((2.0/(3.0*pi)))*sin(3.0*x)
    four = 0.5 + (2.0/pi)*sin(x) + ((2.0/(3.0*pi)))*sin(3.0*x) + ((2.0/(5.0*pi))*sin(5.0*x))

    do j = 1, n, 2
        alot = alot + (2.0/pi)*(1.0/j)*sin(j*x)
    enddo


    write(20, *) x, two
    write(21, *) x, three
    write(22, *) x, four
    write(23, *) x, alot

    x = x + xstep
enddo


end

