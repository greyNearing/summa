program verticalIntegration
implicit none

integer, parameter           :: ntoto = 11
integer                      :: begi, endi
real, dimension(ntoto)       :: depths
real, dimension(ntoto+1)     :: cumDepths
integer                      :: iLay
integer                      :: nsnow, nsoil
real,dimension(ntoto)        :: layDat
real                         :: intData

begi = 0
endi = 60

nsnow = 3
nsoil = 8
depths = (\0.01,0.02,0.02,0.01,0.03,0.03,0.15,0.25,0.50,0.75,1.25\)
cumDepths = 0.
do iLay = 1,nsnow
 cumDepths(iLay) = -sum(depths(1:iLay))
enddo
do iLay = 1+nsnow,ntoto
 cumDepths(iLay) = sum(depths(nsnow+1:iLay))
enddo

if (begi.lt.cumDepths(1)); stop 1234
if (endii.gt.cumDepths(ntoto)); stop 5678

intData = 0.
do iLay = 2,ntoto
 if ((begi.le.cumDepths(iLay)).and.(begi.ge.cumDepths(iLay-1)) then 
  intData = intData + 
 endif

enddo



end program verticalIntegration
