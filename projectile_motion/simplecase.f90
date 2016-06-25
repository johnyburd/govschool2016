program simplecase

real :: s(3), v(3), a(3), vp(3), sp(3), theta, iv, pi, speed, windvel(3), vrel(3)
real :: g, m, airden, sa, c
real :: t, tau
integer :: i, n

pi = 4 * atan(1.0)
g = 9.8

airden = 1.225
m = 0.06782
c = 0.47
sa = pi * 0.012745**2

iv = 5.103
theta = (22.5 * pi) / 180.0

a(1) = 0.0
a(2) = 0.0
a(3) = -g

windvel = 0.0
!windvel(2) = 10.0

v(1) = iv * cos(theta)
v(2) = 0.0
v(3) = iv * sin(theta)

s(1) = 0.0
s(2) = 0.0
s(3) = 1.1

vp = v
sp = s

t = 0.0
n = 5000.0
tau = 2.0/n

open(unit = 15, file='xpos.dat')
open(unit = 16, file='zpos.dat')
open(unit = 17, file='xvel.dat')
open(unit = 18, file='zvel.dat')
open(unit = 19, file='xacc.dat')
open(unit = 20, file='zacc.dat')
open(unit = 21, file='zvsx.dat')

do while (s(3) > 0.0)

    vrel = v - windvel

    speed = norm2(v)

    a =- (0.5 * c * sa * v * speed * airden)/m
    a(3) = a(3) - g

    a = a - (((-0.5) * c * sa * airden)/m) * norm2(vrel) * vrel

    v = v + a * tau
    !sp = sp + (vp * t) - (0.5 * a * t**2)
    sp = sp  + (vp*tau)

    !s = s + ((v * t) -(0.5 * a *t**2))
    s = s  + (v * tau)
    vp = vp + a * tau

    write(15, *) t, (s(1)+sp(1))/2.0
    write(16, *) t, (s(3)+sp(3))/2.0
    write(17, *) t, (v(1)+vp(1))/2.0
    write(18, *) t, (v(3)+vp(3))/2.0
    write(19, *) t, a(1)
    write(20, *) t, a(3)
    write(21, *) (s(1)+sp(1))/2.0, (s(2)+sp(2))/2.0, (s(3)+sp(3))/2.0


    t = t + tau
enddo

end
