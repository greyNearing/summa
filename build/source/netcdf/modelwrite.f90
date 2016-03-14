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
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:attr_data,attr_meta                   ! local attributes
 USE data_struc,only:type_data,type_meta                   ! local classification of veg, soil, etc.
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookSTAT                             ! number of output statistics
 USE var_lookup,only:maxvarAttr,maxvarType                 ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! local variables
 integer(i4b)                :: iVar                       ! loop through variables
 ! initialize error control
 err=0;message="f-writeAttrb/"

 ! loop through local attributes
 do iVar=1,maxvarAttr
  ! check that the variable is desired
  if (.not.attr_meta(iVar)%stat(iLookSTAT%inst)) cycle
  ! initialize message
  message=trim(message)//trim(attr_meta(iVar)%varname)//'/'
  ! write data
  err = nf90_put_var(ncid,attr_meta(iVar)%ncVarID(iLookSTAT%inst),(/attr_data%var(iVar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeAttrb/"
 end do  ! looping through local attributes

 ! loop through local classification of veg, soil, etc.
 do iVar=1,maxvarType
  ! check that the variable is desired
  if (.not.type_meta(iVar)%stat(iLookSTAT%inst)) cycle
  ! initialize message
  message=trim(message)//trim(type_meta(iVar)%varname)//'/'
  ! write data
  err = nf90_put_var(ncid,type_meta(iVar)%ncVarID(iLookSTAT%inst),(/type_data%var(iVar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeAttrb/"
 end do  ! looping through local classification of veg, soil, etc.

 end subroutine writeAttrb


 ! **********************************************************************************************************
 ! public subroutine writeParam: write model parameters
 ! **********************************************************************************************************
 subroutine writeParam(iHRU,err,message)
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:mpar_data,mpar_meta                   ! local-column model parameter structures
 USE data_struc,only:bpar_data,bpar_meta                   ! basin-average model parameter structures
 USE var_lookup,only:iLookSTAT                             ! number of output statistics
 USE var_lookup,only:maxvarMpar,maxvarBpar                 ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: iHRU                       ! hydrologic response unit
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! local variables
 integer(i4b)                :: ipar                       ! loop through model parameters
 ! initialize error control
 err=0;message="f-writeParam/"

 ! loop through local column model parameters
 do ipar=1,maxvarMpar
  ! check that the variable is desired
  if (.not.mpar_meta(ipar)%stat(iLookSTAT%inst)) cycle
  ! initialize message
  message=trim(message)//trim(mpar_meta(ipar)%varname)//'/'
  ! write data
  err = nf90_put_var(ncid,mpar_meta(ipar)%ncVarID(iLookSTAT%inst),(/mpar_data%var(ipar)/),start=(/iHRU/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeParam/"
 end do  ! looping through local column model parameters

 ! loop through basin-average model parameters
 do ipar=1,maxvarBpar
  ! check that the variable is desired
  if (.not.bpar_meta(ipar)%stat(iLookSTAT%inst)) cycle
  ! initialize message
  message=trim(message)//trim(bpar_meta(ipar)%varname)//'/'
  ! write data
  err = nf90_put_var(ncid,bpar_meta(ipar)%ncVarID(iLookSTAT%inst),(/bpar_data%var(ipar)/),start=(/1/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  ! re-initialize message
  message="f-writeParam/"
 end do  ! looping through basin-average model parameters

 end subroutine writeParam


 ! **********************************************************************************************************
 ! public subroutine writeForce: write model forcing data
 ! **********************************************************************************************************
 subroutine writeForce(iHRU,istep,err,message)
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:forc_stat,forc_meta                   ! forcing data structures
 USE data_struc,only:maxVarStat                           ! number of output statistics
 USE var_lookup,only:iLookFORCE                            ! identifies element of the forcing structure
 USE var_lookup,only:iLookSTAT                             ! identifies output statistic
 USE var_lookup,only:maxvarForc                            ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: iHRU                       ! hydrologic response unit
 integer(i4b), intent(in)    :: istep                      ! model time step
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! pointers to time variables
 real(dp),pointer            :: dtime                      ! time since reference time (seconds)
 ! local variables
 integer(i4b)                :: iforce                     ! loop through model forcing variables
 integer(i4b)                :: istat                      ! loop through output statistics
 ! initialize error control
 err=0;message="f-writeForce/"

 ! assign pointers
 dtime => forc_stat(iHRU)%var(iLookFORCE%time)%dat(iLookSTAT%inst)

 ! write the time coordinate variable
 if(iHRU == 1)then
  message=trim(message)//'writeTime/'
  !print*, 'iHRU, istep, dtime = ', iHRU, istep, dtime
  err = nf90_put_var(ncid,forc_meta(iLookFORCE%time)%ncVarID(iLookSTAT%inst),(/dtime/),start=(/istep/),count=(/1/))
  call netcdf_err(err,message); if (err/=0) return
  message="f-writeForce/"
 endif

 ! loop through model forcing variables
 do iforce=1,maxvarForc
  ! loop through output statistics
  do istat=1,maxVarStat 
   ! ignore the time variable (used as a coordinate variable above)
   if(forc_meta(iforce)%varname == 'time') cycle
   ! check that the variable is desired
   if (.not.forc_meta(iforce)%stat(istat)) cycle
   ! initialize message
   message=trim(message)//trim(forc_meta(iforce)%varname)//'/'
   ! write data
   err = nf90_put_var(ncid,forc_meta(iforce)%ncVarID(istat),(/forc_stat(iHRU)%var(iforce)%dat(istat)/),start=(/iHRU,istep/),count=(/1,1/))
   call netcdf_err(err,message); if (err/=0) return
  end do  ! looping through output statistics
 end do  ! looping through forcing data variables

 end subroutine writeForce

 ! **********************************************************************************************************
 ! public subroutine writeModel: write local column model variables
 ! **********************************************************************************************************
 subroutine writeModel(iHRU,istep,err,message)
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:indx_data,indx_meta                   ! index data structures
 USE data_struc,only:mvar_stat,mvar_data,mvar_meta         ! model data structures
 USE data_struc,only:maxVarStat                            ! number of output statistics
 USE var_lookup,only:iLookINDEX                            ! identifies element of the index structure
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookSTAT                             ! identifies output statistic
 USE var_lookup,only:maxvarIndx,maxvarMvar                 ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)   :: iHRU                       ! hydrologic response unit
 integer(i4b), intent(in)   :: istep                      ! model time step
 integer(i4b),intent(out)   :: err                        ! error code
 character(*),intent(out)   :: message                    ! error message
 ! declare pointers to the model index variables (used to identify the appropriate write position)
 integer(i4b),pointer       :: nSnow                          ! number of snow layers
 integer(i4b),pointer       :: nSoil                          ! number of soil layers
 integer(i4b),pointer       :: nLayers                        ! total number of layers
 integer(i4b),pointer       :: midSnowStartIndex              ! start index of the midSnow vector for a given timestep
 integer(i4b),pointer       :: midSoilStartIndex              ! start index of the midSoil vector for a given timestep
 integer(i4b),pointer       :: midTotoStartIndex              ! start index of the midToto vector for a given timestep
 integer(i4b),pointer       :: ifcSnowStartIndex              ! start index of the ifcSnow vector for a given timestep
 integer(i4b),pointer       :: ifcSoilStartIndex              ! start index of the ifcSoil vector for a given timestep
 integer(i4b),pointer       :: ifcTotoStartIndex              ! start index of the ifcToto vector for a given timestep
 ! local variables
 integer(i4b)               :: iindex                     ! loop through model index variables
 integer(i4b)               :: imodel                     ! loop through model variables
 integer(i4b)               :: istat                      ! loop through output statistics
 ! initialize error control
 err=0;message="f-writeModel/"

 ! assign pointers to model layers
 nSoil   => indx_data%var(iLookINDEX%nSoil)%dat(1)
 nSnow   => indx_data%var(iLookINDEX%nSnow)%dat(1)
 nLayers => indx_data%var(iLookINDEX%nLayers)%dat(1)

 ! assign pointers to model indices
 midSnowStartIndex => indx_data%var(iLookINDEX%midSnowStartIndex)%dat(1)
 midSoilStartIndex => indx_data%var(iLookINDEX%midSoilStartIndex)%dat(1)
 midTotoStartIndex => indx_data%var(iLookINDEX%midTotoStartIndex)%dat(1)
 ifcSnowStartIndex => indx_data%var(iLookINDEX%ifcSnowStartIndex)%dat(1)
 ifcSoilStartIndex => indx_data%var(iLookINDEX%ifcSoilStartIndex)%dat(1)
 ifcTotoStartIndex => indx_data%var(iLookINDEX%ifcTotoStartIndex)%dat(1)

 ! loop through model index variables
 ! ----------------------------------
 do iindex=1,maxvarIndx
  ! check that the variable is desired
  if (.not.indx_meta(iindex)%stat(iLookSTAT%inst)) cycle
  ! initialize message
  message=trim(message)//trim(indx_meta(iindex)%varname)//'/'
  ! write data
  err = nf90_put_var(ncid,indx_meta(iindex)%ncVarID(iLookSTAT%inst),indx_data%var(iindex)%dat,start=(/iHRU,istep/),count=(/1,1/))
  call netcdf_err(err,message); if (err/=0) return
  message="f-writeModel/"
 end do

 ! loop through model variables
 ! ----------------------------
 do imodel=1,maxvarMvar
  ! loop through output statistics
  do istat=1,maxVarStat 
   ! check that the variable is desired
   if (.not.mvar_meta(imodel)%stat(istat)) cycle
   ! initialize message
   message=trim(message)//trim(mvar_meta(imodel)%varname)//'/'
   ! write data
   select case(mvar_meta(imodel)%vartype)
    case(iLookVarType%scalarv); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),(/mvar_stat(iHRU)%var(imodel)%dat(istat)/),start=(/iHRU,istep/),count=(/1,1/))    ! scalarv
    case(iLookVarType%wLength); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,1,istep/),count=(/1,maxSpectral,1/))       ! wLength
    case(iLookVarType%midSnow); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,midSnowStartIndex/),count=(/1,nSnow/))     ! midSnow
    case(iLookVarType%midSoil); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,midSoilStartIndex/),count=(/1,nSoil/))     ! midSoil
    case(iLookVarType%midToto); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,midTotoStartIndex/),count=(/1,nLayers/))   ! midToto
    case(iLookVarType%ifcSnow); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,ifcSnowStartIndex/),count=(/1,nSnow+1/))   ! ifcSnow
    case(iLookVarType%ifcSoil); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,ifcSoilStartIndex/),count=(/1,nSoil+1/))   ! ifcSoil
    case(iLookVarType%ifcToto); err = nf90_put_var(ncid,mvar_meta(imodel)%ncVarID(istat),mvar_data%var(imodel)%dat,start=(/iHRU,ifcTotoStartIndex/),count=(/1,nLayers+1/)) ! ifcToto
    case default
     err=40; message=trim(message)//"unknownVariableType[name='"//trim(mvar_meta(imodel)%varname)//"'; &
                                    &type='"//trim(get_varTypeName(mvar_meta(imodel)%vartype))//"']"; return
   endselect
   call netcdf_err(err,message); if (err/=0) return
   message="f-writeModel/"
  end do  ! looping through output statistics
 end do  ! looping through model variables

 end subroutine writeModel


 ! **********************************************************************************************************
 ! public subroutine writeBasin: write basin-average variables
 ! **********************************************************************************************************
 subroutine writeBasin(istep,err,message)
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:bvar_data,bvar_meta,bvar_stat         ! model data structures
 USE data_struc,only:maxVarStat                            ! loop sizes (to avoid using size() on a pointer)
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookSTAT                             ! identifies output statistic
 USE var_lookup,only:maxvarBvar                            ! loop sizes (to avoid using size() on a pointer)
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)    :: istep                      ! model time step
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 ! local variables
 integer(i4b)                :: imodel                     ! loop through model variables
 integer(i4b)                :: istat                      ! loop through output statistics
 ! initialize error control
 err=0;message="f-writeModel/"

 ! loop through model variables
 ! ----------------------------
 do imodel=1,maxvarBvar
  ! loop through output statistics
  do istat=1,maxVarStat 
   ! check that the variable is desired
   if (.not.bvar_meta(imodel)%stat(istat)) cycle
   ! initialize message
   message=trim(message)//trim(bvar_meta(imodel)%varname)//'/'
   ! write data
   select case(bvar_meta(imodel)%vartype)
    case(iLookVarType%scalarv); err = nf90_put_var(ncid,bvar_meta(imodel)%ncVarID(iLookSTAT%inst),(/bvar_stat%var(imodel)%dat(istat)/),start=(/istep/),count=(/1/)) ! scalarv
    case(iLookVarType%routing)                                                                                        ! routing
     if(istep==1)then
      err = nf90_put_var(ncid,bvar_meta(imodel)%ncVarID(iLookSTAT%inst),bvar_data%var(imodel)%dat,start=(/1/),count=(/1000/))
     endif
    case default
     err=40; message=trim(message)//"unknownVariableType[name='"//trim(bvar_meta(imodel)%varname)//"'; &
                                    &type='"//trim(get_varTypeName(bvar_meta(imodel)%vartype))//"']"; return
   endselect
   call netcdf_err(err,message); if (err/=0) return
   message="f-writeBasin/"
  end do  ! loopting through output statistics
 end do  ! looping through model variables

 end subroutine writeBasin

 ! **********************************************************************************************************
 ! public subroutine writeInteg: write vertically integrated model variables
 ! **********************************************************************************************************
 subroutine writeInteg(iHRU,istep,err,message)
 USE data_struc,only:ncid                                  ! ID for netcdf output file 
 USE data_struc,only:intg_stat,intg_meta                   ! model data structures
 USE data_struc,only:maxVarStat                            ! number of output statistics
 USE data_struc,only:maxIntLayr                            ! number of integrated layers
 USE var_lookup,only:maxVarIntg                            ! loop sizes (to avoid using size() on a pointer)
 USE var_lookup,only:iLookIntg                             ! identifies element of the index structure
 USE var_lookup,only:iLookVarType                          ! look up for type case select 
 USE var_lookup,only:iLookSTAT                             ! identifies output statistic
 USE get_ixname_module,only:get_varTypeName                ! string names of different data types for error message
 implicit none
 ! declare dummy variables
 integer(i4b), intent(in)   :: iHRU                       ! hydrologic response unit
 integer(i4b), intent(in)   :: istep                      ! model time step
 integer(i4b),intent(out)   :: err                        ! error code
 character(*),intent(out)   :: message                    ! error message
 ! local variables
 integer(i4b)               :: iVar                       ! loop through model variables
 integer(i4b)               :: iInt                       ! loop through integrated variables
 integer(i4b)               :: iStat                      ! loop through output statistics
 ! initialize error control
 err=0;message="writeInteg/"

 ! loop through integrated variables
 ! ----------------------------
 do iVar=1,maxVarIntg  ! loop through variables
  do iInt=1,maxIntLayr  ! loop through integrated layers
   do iStat=1,maxVarStat  ! loop through output statistics
    if (.not.intg_meta(iVar,iInt)%stat(iStat)) cycle ! check that the variable is desired
     message=trim(message)//trim(intg_meta(iVar,iInt)%varname)//'/'
     err = nf90_put_var(ncid,intg_meta(iVar,iInt)%ncVarID(iStat),(/intg_stat(iHRU,iInt)%var(iVar)%dat(iStat)/), &
                        start=(/iHRU,istep/),count=(/1,1/))
     call netcdf_err(err,message); if (err/=0) return
     message="writeInteg/"
   enddo  ! looping through output statistics
  enddo ! integrated layers
 enddo  ! looping through model variables

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
 end subroutine netcdf_err


end module modelwrite_module
