program springNum

implicit none
real tstep, t, pi, v, pos, c
real ix, iv, m, k
integer i, n

pi = atan(1.0) * 4.0
n = 500
tstep = (4 * pi) / float(n)
t = 0.0

ix = 0.4
k = 1.0
m = 1.0
iv = 0.0

c = sqrt(m/k)

open(unit = 20, file='posNum.dat')
open(unit = 21, file='velNum.dat')

do i = 1, n

    pos = iv * c * sin(c * t) + ix * cos(c * t)

    v = iv * (c**2) * cos(c*t) - (ix * c * sin(c*t))

    write(20, *) t, pos
    write(21, *) t, v

    t = t + tstep
enddo

end

