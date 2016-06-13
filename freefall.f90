program FreeFall
Implicit None

REAL y, v, tau, a, t, g
REAL yp, vp, my, mv
g = -9.8
t = 0.0
y = 57.0
yp = 57.0
v = -3.8
vp = -3.8
tau = 0.001

read (*,*) y
read (*,*) v
mv = v
my = y

open(unit = 15, file = 'freefallPos.dat')
open(unit = 20, file = 'freefallVel.dat')

do while (y > 0.0)

    a = g - (0.5*0.47*.0044*mv*abs(mv)*1.225)/0.15

    !a = g / 150

    y = y + tau * v
    v = v + tau * a


    vp = vp + tau * a

    yp = yp + tau * vp


    t = t + tau

    my = (y+yp)/2.0
    mv = (v+vp)/2.0

    !mv = mv - (0.5*0.47*0.018*mv*abs(mv)*1225)/150
!    a = a + 0.5*0.47*.018*mv*abs(mv)*150*1225

    write(15,*) t, my
    write(20,*) t, mv
enddo


end
