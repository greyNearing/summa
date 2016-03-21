! SUMMA - Structure for Unifying Multiple Modeling Alternatives
! Copyright (C) 2014-2015 NCAR/RAL
!
! This file is part of SUMMA
!
! For more information see: http://www.ral.ucar.edu/projects/summa
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module modelwrite_module
USE nrtype
USE netcdf
implicit none
private
public::writeForce
public::writeAttrb
public::writeParam
public::writeModel
public::writeBasin
public::writeInteg
! define dimension lengths
integer(i4b),parameter      :: maxSpectral=2              ! maximum number of spectral bands
contains


 ! **********************************************************************************************************
 ! public subroutine writeAttrb: write local attributes
 ! **********************************************************************************************************
 subroutine writeAttrb(iHRU,err,message)
 USE data_struc,only:attr_data,attr_meta                   ! local attributes
 USE data_struc,only:type_data,type_meta                   ! local classification of veg, soil, etc.
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookStat                             ! number of output statistics
 USE var_lookup,only:maxVarAttr,maxVarType                 ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! local variables
 integer(i4b)                :: ncid,varid                 ! netcdf indexes 
 integer(i4b)                :: iVar                       ! loop through variables
 integer(i4b),parameter      :: modelTime=1                ! model timestep frequency 
 ! initialize error control
 err=0;message="f-writeAttrb/"

 ! loop through local attributes
 do iVar=1,maxVarAttr
  ! check that the variable is desired
  if (.not.attr_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle
  ! initialize message
  message=trim(message)//trim(attr_meta(iVar)%varName)//'/'
  ! write data
  ncid  = attr_meta(iVar)%ncFilID(modelTime)
  varid = attr_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,(/attr_data%var(iVar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeAttrb/"
 enddo  ! looping through local attributes

 ! loop through local classification of veg, soil, etc.
 do iVar=1,maxVarType
  ! check that the variable is desired
  if (.not.type_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle
  ! initialize message
  message=trim(message)//trim(type_meta(iVar)%varName)//'/'
  ! write data
  ncid  = type_meta(iVar)%ncFilID(modelTime)
  varid = type_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,(/type_data%var(iVar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeAttrb/"
 enddo  ! looping through local classification of veg, soil, etc.

 return
 end subroutine writeAttrb


 ! **********************************************************************************************************
 ! public subroutine writeParam: write model parameters
 ! **********************************************************************************************************
 subroutine writeParam(iHRU,err,message)
 USE data_struc,only:mpar_data,mpar_meta                   ! local-column model parameter structures
 USE data_struc,only:bpar_data,bpar_meta                   ! basin-average model parameter structures
 USE var_lookup,only:iLookStat                             ! number of output statistics
 USE var_lookup,only:maxVarMpar,maxVarBpar                 ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! local variables
 integer(i4b)                :: ncid,varid                 ! netcdf indexes 
 integer(i4b),parameter      :: modelTime=1                ! model timestep frequency 
 integer(i4b)                :: iVar                       ! loop through model parameters

 ! initialize error control
 err=0;message="f-writeParam/"

 ! loop through local column model parameters
 do iVar = 1,maxVarMpar
  ! check that the variable is desired
  if (.not.mpar_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle
  ! initialize message
  message=trim(message)//trim(mpar_meta(iVar)%varName)//'/'
  ! write data
  ncid  = mpar_meta(iVar)%ncFilID(modelTime)
  varid = mpar_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,(/mpar_data%var(iVar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeParam/"
 enddo  ! looping through local column model parameters

 ! loop through basin-average model parameters
 do iVar = 1,maxVarBpar
  ! check that the variable is desired
  if (.not.bpar_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle
  ! initialize message
  message=trim(message)//trim(bpar_meta(iVar)%varName)//'/'
  ! write data
  ncid  = bpar_meta(iVar)%ncFilID(modelTime)
  varid = bpar_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,(/bpar_data%var(iVar)/),start=(/1/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeParam/"
 enddo  ! looping through basin-average model parameters

 return
 end subroutine writeParam


 ! **********************************************************************************************************
 ! public subroutine writeForce: write model forcing data
 ! **********************************************************************************************************
 subroutine writeForce(iHRU,iFreq,iStep,err,message)
 USE data_struc,only:nFreq                                 ! number of output frequencies 
 USE data_struc,only:forc_stat,forc_data,forc_meta         ! forcing data structures
 USE data_struc,only:maxVarStat                            ! number of output statistics
 USE var_lookup,only:iLookForce                            ! identifies element of the forcing structure
 USE var_lookup,only:iLookStat                             ! identifies output statistic
 USE var_lookup,only:maxVarForc                            ! loop sizes (to avoid using size() on a pointer)
 implicit none

 ! declare dummy variables
 integer(i4b),intent(in)     :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(in)     :: iStep                      ! model time step
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 integer(i4b),intent(in)     :: iFreq                      ! output frequency 

 ! local variables
 real(dp)                    :: dtime                      ! seconds since reference
 integer(i4b)                :: ncid,varid                 ! netcdf indexes 
 integer(i4b)                :: iVar                       ! loop through model parameters
 integer(i4b)                :: iStat                      ! loop through output statistics

 ! initialize error control
 err=0;message="f-writeForce/"

 ! assign pointers
 dtime = forc_data%var(iLookFORCE%time)

 ! write the time coordinate variable
 if (iHRU.eq.1) then
  message = trim(message)//'writeTime/'
  ncid  = forc_meta(iLookForce%time)%ncFilID(iFreq)
  varid = forc_meta(iLookForce%time)%ncVarID(iFreq,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,(/dtime/),start=(/iStep/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  message="f-writeForce/"
 endif

 ! loop through model forcing variables
 do iVar = 1,maxVarForc
  if(forc_meta(iVar)%varName.eq.'time') cycle ! ignore the time variable (used as a coordinate variable above)
  message=trim(message)//trim(forc_meta(iVar)%varName)//'/'! initialize message
  do iStat = 1,maxVarStat 
   if (.not.forc_meta(iVar)%statFlg(iFreq,iStat)) cycle ! check that the variable is desired
   ncid  = forc_meta(ivar)%ncFilID(iFreq)
   varid = forc_meta(ivar)%ncVarID(iFreq,iStat,1)
   err = nf90_put_var(ncid,varid,(/forc_stat(iHRU,iFreq)%var(iVar)%dat(iStat)/),start=(/iHRU,iStep/),count=(/1,1/))
   call netcdf_err(err,message); if (err/=0) return
  enddo  ! iStat 
 enddo  ! iVar 

 return
 end subroutine writeForce

 ! **********************************************************************************************************
 ! public subroutine writeModel: write local column model variables
 ! **********************************************************************************************************
 subroutine writeModel(iHRU,iFreq,iStep,err,message)
 USE data_struc,only:nFreq                                 ! number of output frequencies 
 USE data_struc,only:indx_data,indx_meta                   ! index data structures
 USE data_struc,only:mvar_stat,mvar_data,mvar_meta         ! model data structures
 USE data_struc,only:maxVarStat                            ! number of output statistics
 USE var_lookup,only:iLookIndex                            ! identifies element of the index structure
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookStat                             ! identifies output statistic
 USE var_lookup,only:maxVarIndx,maxVarMvar                 ! loop sizes (to avoid using size() on a pointer)
 implicit none

 ! declare dummy variables
 integer(i4b), intent(in)   :: iHRU                        ! hydrologic response unit
 integer(i4b),intent(in)    :: iFreq                       ! output frequency 
 integer(i4b), intent(in)   :: iStep                       ! model time step
 integer(i4b),intent(out)   :: err                         ! error code
 character(*),intent(out)   :: message                     ! error message

 ! declare pointers to the model index variables (used to identify the appropriate write position)
 integer(i4b),pointer       :: nSnow                       ! number of snow layers
 integer(i4b),pointer       :: nSoil                       ! number of soil layers
 integer(i4b),pointer       :: nLayers                     ! total number of layers
 integer(i4b),pointer       :: midSnowStartIndex           ! start index of the midSnow vector for a given timestep
 integer(i4b),pointer       :: midSoilStartIndex           ! start index of the midSoil vector for a given timestep
 integer(i4b),pointer       :: midTotoStartIndex           ! start index of the midToto vector for a given timestep
 integer(i4b),pointer       :: ifcSnowStartIndex           ! start index of the ifcSnow vector for a given timestep
 integer(i4b),pointer       :: ifcSoilStartIndex           ! start index of the ifcSoil vector for a given timestep
 integer(i4b),pointer       :: ifcTotoStartIndex           ! start index of the ifcToto vector for a given timestep

 ! local variables
 integer(i4b)               :: ncid,varid                  ! netcdf indexes 
 integer(i4b)               :: iVar                        ! loop through variables
 integer(i4b)               :: iStat                       ! loop through output statistics
 integer(i4b),parameter     :: modelTime=1                 ! model timestep frequency 

 ! initialize error control
 err=0;message="f-writeModel/"

 ! assign pointers to model layers
 nSoil   => indx_data%var(iLookIndex%nSoil)%dat(1)
 nSnow   => indx_data%var(iLookIndex%nSnow)%dat(1)
 nLayers => indx_data%var(iLookIndex%nLayers)%dat(1)

 ! assign pointers to model indices
 midSnowStartIndex => indx_data%var(iLookIndex%midSnowStartIndex)%dat(1)
 midSoilStartIndex => indx_data%var(iLookIndex%midSoilStartIndex)%dat(1)
 midTotoStartIndex => indx_data%var(iLookIndex%midTotoStartIndex)%dat(1)
 ifcSnowStartIndex => indx_data%var(iLookIndex%ifcSnowStartIndex)%dat(1)
 ifcSoilStartIndex => indx_data%var(iLookIndex%ifcSoilStartIndex)%dat(1)
 ifcTotoStartIndex => indx_data%var(iLookIndex%ifcTotoStartIndex)%dat(1)

 ! loop through model index variables
 ! ----------------------------------
 do iVar = 1,maxVarIndx
  message=trim(message)//trim(indx_meta(iVar)%varName)//'/' ! initialize message
  if (.not.indx_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle  ! check that the variable is desired
  ncid  = indx_meta(iVar)%ncFilID(modelTime)
  varid = indx_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
  err = nf90_put_var(ncid,varid,indx_data%var(iVar)%dat,start=(/iHRU,iStep/),count=(/1,1/))
  call netcdf_err(err,message); if (err/=0) return
  message="f-writeModel/"
 enddo

 ! loop through model variables
 ! ----------------------------
 do iVar = 1,maxVarMvar
  message=trim(message)//trim(mvar_meta(iVar)%varName)//'/'
  if (mvar_meta(iVar)%varType.eq.iLookVarType%scalarv) then
   do iStat=1,maxVarStat 
    if (.not.mvar_meta(iVar)%statFlg(iFreq,iStat)) cycle
    ncid  = mvar_meta(iVar)%ncFilID(iFreq)
    varid = mvar_meta(iVar)%ncVarID(iFreq,iStat,1)
    err = nf90_put_var(ncid,varid,(/mvar_stat(iHRU,iFreq)%var(iVar)%dat(iStat)/),start=(/iHRU,iStep/),count=(/1,1/))   
   enddo ! iStat
  else
   if (.not.mvar_meta(iVar)%statFlg(iFreq,iLookStat%inst)) cycle
   ncid  = mvar_meta(ivar)%ncFilID(modelTime)
   varid = mvar_meta(ivar)%ncVarID(modelTime,iLookStat%inst,1)
   select case(mvar_meta(iVar)%varType)
    case(iLookVarType%wLength)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,1,iStep/),count=(/1,maxSpectral,1/))       
    case(iLookVarType%midSnow)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,midSnowStartIndex/),count=(/1,nSnow/))   
    case(iLookVarType%midSoil)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,midSoilStartIndex/),count=(/1,nSoil/))  
    case(iLookVarType%midToto)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,midTotoStartIndex/),count=(/1,nLayers/)) 
    case(iLookVarType%ifcSnow)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,ifcSnowStartIndex/),count=(/1,nSnow+1/)) 
    case(iLookVarType%ifcSoil)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,ifcSoilStartIndex/),count=(/1,nSoil+1/)) 
    case(iLookVarType%ifcToto)
     err = nf90_put_var(ncid,varid,mvar_data%var(iVar)%dat,start=(/iHRU,ifcTotoStartIndex/),count=(/1,nLayers+1/))
   endselect
  endif  ! scalarv
  call netcdf_err(err,message); if (err/=0) return
  message="f-writeModel/"
 enddo  ! looping through model variables

 return
 end subroutine writeModel


 ! **********************************************************************************************************
 ! public subroutine writeBasin: write basin-average variables
 ! **********************************************************************************************************
 subroutine writeBasin(iFreq,iStep,err,message)
 USE data_struc,only:nFreq                                 ! number of output frequencies 
 USE data_struc,only:bvar_data,bvar_meta,bvar_stat         ! model data structures
 USE data_struc,only:maxVarStat                            ! loop sizes (to avoid using size() on a pointer)
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookStat                             ! identifies output statistic
 USE var_lookup,only:maxVarBvar                            ! loop sizes (to avoid using size() on a pointer)
 implicit none

 ! declare dummy variables
 integer(i4b),intent(in)    :: iFreq                       ! output frequency 
 integer(i4b), intent(in)   :: iStep                       ! model time step
 integer(i4b),intent(out)   :: err                         ! error code
 character(*),intent(out)   :: message                     ! error message

 ! local variables
 integer(i4b)               :: ncid,varid                  ! netcdf indexes 
 integer(i4b)               :: iVar                        ! loop through variables
 integer(i4b)               :: iStat                       ! loop through output statistics
 integer(i4b),parameter     :: modelTime=1                 ! model timestep frequency 

 ! initialize error control
 err=0;message="f-writeBasin/"

 ! loop through model variables
 ! ----------------------------
 do iVar = 1,maxVarBvar
  message=trim(message)//trim(bvar_meta(iVar)%varName)//'/'
  if ((bvar_meta(iVar)%varType.eq.iLookVarType%routing).and.(iStep.eq.1)) then 
   if (.not.bvar_meta(iVar)%statFlg(iFreq,iLookStat%inst)) cycle
   ncid  = bvar_meta(iVar)%ncFilID(modelTime)
   varid = bvar_meta(iVar)%ncVarID(modelTime,iLookStat%inst,1)
   err = nf90_put_var(ncid,varid,(/bvar_data%var(iVar)%dat(iLookStat%inst)/),start=(/1/),count=(/1000/))
   call netcdf_err(err,message); if (err/=0) return
   cycle
  endif
  message="f-writeBasin/"
  do iStat = 1,maxVarStat 
   ncid  = bvar_meta(iVar)%ncFilID(iFreq)
   varid = bvar_meta(iVar)%ncVarID(iFreq,iStat,1)
   if (.not.bvar_meta(iVar)%statFlg(iFreq,iStat)) cycle
   if (bvar_meta(iVar)%varType.eq.iLookVarType%scalarv) then
    err = nf90_put_var(ncid,varid,(/bvar_stat(iFreq)%var(iVar)%dat(iStat)/),start=(/iStep/),count=(/1/)) 
    call netcdf_err(err,message); if (err/=0) return
   endif
  enddo ! iStat
 enddo ! iVar

 return
 end subroutine writeBasin

 ! **********************************************************************************************************
 ! public subroutine writeInteg: write vertically integrated model variables
 ! **********************************************************************************************************
 subroutine writeInteg(iHRU,iFreq,iStep,err,message)
 USE data_struc,only:nFreq                                 ! number of output frequencies 
 USE data_struc,only:intg_stat,intg_meta                   ! model data structures
 USE data_struc,only:indx_data                             ! model data structures
 USE data_struc,only:maxVarStat                            ! number of output statistics
 USE var_lookup,only:maxVarIntg                            ! loop sizes (to avoid using size() on a pointer)
 USE var_lookup,only:iLookIntg                             ! identifies element of the index structure
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookStat                             ! identifies output statistic
 USE var_lookup,only:iLookIndex                            ! identifies output statistic
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 implicit none

 ! declare dummy variables
 integer(i4b), intent(in)   :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(in)    :: iFreq                      ! output frequency 
 integer(i4b), intent(in)   :: iStep                      ! model time step
 integer(i4b),intent(out)   :: err                        ! error code
 character(*),intent(out)   :: message                    ! error message

 ! local variables
 integer(i4b)               :: iVar                       ! loop through model variables
 integer(i4b)               :: iLay                       ! loop through integrated layers
 integer(i4b)               :: iStat                      ! loop through output statistics
 integer(i4b)               :: ncid,varid                 ! netcdf indexes 
 integer(i4b)               :: nLayers                    ! total number of layers

 ! initialize error control
 err=0;message="writeInteg/"

 ! number of integrated layers
 nLayers = 1+indx_data%var(iLookIndex%nSoil)%dat(1)

 ! loop through integrated variables
 ! ----------------------------
 do iVar = 1,maxVarIntg
  do iStat = 1,maxVarStat 
   if (.not.intg_meta(iVar)%statFlg(iFreq,iStat)) cycle
   do iLay = 1,nLayers
    ncid  = intg_meta(iVar)%ncFilID(iFreq)
    varid = intg_meta(iVar)%ncVarID(iFreq,iStat,iLay)
    err = nf90_put_var(ncid,varid,(/intg_stat(iHRU,iFreq,iLay)%var(iVar)%dat(iStat)/),start=(/iHRU,1,iStep/),count=(/1,nLayers,1/)) 
    call netcdf_err(err,message)
    if (err/=0) then; message=trim(message)//trim(intg_meta(iVar)%varName)//'/'; return; endif
   enddo  ! iLay 
  enddo  ! iStat 
 enddo  ! iVar 

 return
 end subroutine writeInteg

 ! **********************************************************************************************************
 ! private subroutine netcdf_err: error control
 ! **********************************************************************************************************
 subroutine netcdf_err(err,message)
 ! used to handle errors for NetCDF calls
 implicit none
 ! declare dummies
 integer(i4b), intent(inout)   :: err
 character(*), intent(inout)   :: message
 ! start procedure here
 if (err/=nf90_noerr) then
  message=trim(message)//"["//trim(nf90_strerror(err))//"]"
  err=200
 else
  err=0
 endif
 
 return
 end subroutine netcdf_err

end module modelwrite_module
