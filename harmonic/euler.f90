program Euler
implicit none

real tstep, t, pi, pos, v, a
real  m, k
integer i, n

pi = atan(1.0) * 4.0
n = 500
tstep = (4*pi) / float(n)
t = 0.0


pos = 0.4
k = 1.0
m = 1.0
v = 0.0


open(unit=20, file='posEul.dat')
open(unit=21, file='velEul.dat')

do i = 1, n

    a = -(k/m) * pos

    pos = pos + tstep * v

    v = v + tstep * a

    write(20, *) t, pos
    write(21, *) t, v

    t = t + tstep
enddo
end
