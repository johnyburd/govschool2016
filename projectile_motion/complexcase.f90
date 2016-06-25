program simplecase
real::s(3),y(3),z(3),vp(3),sp(3),abe,iv,q,abd
real::b,m,abc,sa,c
real::d,abf
integer::i,n
goto 16
12 continue
open(unit=15,file='xpos.dat')
open(unit=16,file='zpos.dat')
open(unit=17,file='xvel.dat')
open(unit=18,file='zvel.dat')
open(unit=19,file='xacc.dat')
open(unit=20,file='zacc.dat')
open(unit=21,file='zvsx.dat')
do while(s(3)>-0.1)
goto 1
19 continue
write(15,*)d,(s(1)+sp(1))/2.0
write(16,*)d,(s(3)+sp(3))/2.0
write(17,*)d,(y(1)+vp(1))/2.0
d=d+abf
goto 34
1 continue
abd=norm2(y)
z(3)=-b-(0.5*c*sa*y(3)*abd*abc)/m
z(1)=-(0.5*c*sa*y(1)*abd*abc)/m
y=y+z*abf
!sp=vp*d-0.5*z*d**2
sp=sp+(vp*d)
!s=y*d-0.5*z*d**2
s=s+(y*d)
vp=vp+z*abf
goto 19
write(18,*)d,(y(3)+vp(3))/2.0
write(19,*)d,z(1)
write(20,*)d,z(3)
write(21,*)(s(1)+sp(1))/2.0,(s(3)+sp(3))/2.0
34 continue
enddo
goto 24
16 continue
q=4*atan(1.0)
b=9.8
abc=1.225
m=0.15
c=0.47
sa=.0044
iv=20.0
abe=(50.0*q)/180.0
z(1)=0.0
z(2)=0.0
z(3)=-b
y(1)=iv*cos(abe)
y(2)=0.0
y(3)=iv*sin(abe)
s(1)=0.0
s(2)=0.0
s(3)=1.0
vp=v
sp=s
d=0.0
n=5000.0
abf=2.0/n
goto 12
24 continue
end
