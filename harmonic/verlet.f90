program Verlet
implicit none

real tau, t, pi, pos, pos2, v, a, temp, b, ke, pe
real m, k, airden, sa, c
integer i, n

c = 1.3
airden = 1.225
m = 0.3
sa = 0.0456


pi = atan(1.0) * 4.0
n = 10000
tau = (2.0*pi) / float(n)
t =0.0
temp = 0.0
b = 0.0

pos = -0.06
k = 8.33
v = -0.34
!v = 0.0

open(unit=20, file='posVer.dat')
open(unit=21, file='velVer.dat')

open(unit=22, file='ke.dat')
open(unit=23, file='pe.dat')
open(unit=24, file="total.dat")

a = -(k/m) * pos
pos2 = (v + tau * a)*tau + pos

do i = 1, n

    !b = 0.5 * m * airden * sa * c * v**2
    !write(*,*) t, a
    b = 0.015
    b = 0.02935

    write(*, *) t, a

    temp = pos2

    a = (-(k/m) * temp) - (b/m)*v !- (k*pos2 + b*v)/m

    pos2 = -pos + 2.0*pos2 + a*(tau**2)


    v = (pos2 - pos)/(2.0*tau)

    ke = 0.5 * m * v**2

    pe = 0.5 * k * pos2**2

    pos = temp

    write(20, *) t, pos2
    write(21, *) t, v

    write(22, *) t, ke
    write(23, *) t, pe
    write(24, *) t, (ke + pe)

    t = t + tau
enddo
end

