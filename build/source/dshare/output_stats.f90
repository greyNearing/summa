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

module module_output
! used to manage output statistics of the model and forcing variables
implicit none
private
public :: alloc_stats
public :: compile_stats
public :: compile_basin_stats
contains

 ! ******************************************************************************************************
 ! public subroutine alloc_stats is called at beginning of simulation to allocate space for output statistics 
 ! ******************************************************************************************************
 subroutine alloc_stats(nHRU,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only: indx_data      ! need the number of soil layers from here
 USE data_struc, only: mvar_stat      ! (to be allocated) model variables
 USE data_struc, only: forc_stat      ! (to be allocated) forcing variables
 USE data_struc, only: bvar_stat      ! (to be allocated) basin variables
 USE data_struc, only: intg_stat      ! (to be allocated) vertically integrated variables
 USE data_struc, only: maxVarStat     ! # of output statistics 
 USE data_struc, only: nFreq          ! # of output frequencies
 USE data_struc, only: ix_soil        ! named variable to identify a soil layer
 USE var_lookup, only: maxVarMvar     ! # of dynamic model variables 
 USE var_lookup, only: maxVarForc     ! # of forcing variables 
 USE var_lookup, only: maxVarBvar     ! # of basin variables 
 USE var_lookup, only: maxVarIntg     ! # of integrated variables 
 USE var_lookup, only: iLookIndex     ! named variables for layer indices 
 implicit none

 ! dummies
 integer(i4b),intent(out) :: err     ! error code
 character(*),intent(out) :: message ! error message
 integer(i4b),intent(in)  :: nHRU    ! number of HRUs

 ! locals
 integer(i4b)             :: iVar    ! index var_info array 
 integer(i4b)             :: iStat   ! index var_stat array 
 integer(i4b)             :: iFreq   ! index into frequency array 
 integer(i4b)             :: iLay    ! index integration layers 
 integer(i4b)             :: iHRU    ! loop through HRUs
 integer(i4b)             :: nLayers ! number of layers 

 ! initialize error control
 err=0; message='alloc_stats/'

 ! number of layers
 nLayers = count(indx_data%var(iLookINDEX%layerType)%dat==ix_soil)+1  

 ! --------------------------------------------
 ! (1) allocate the HRU level structure
 ! --------------------------------------------
 ! model variables
 if(associated(mvar_stat)) deallocate(mvar_stat)
 allocate(mvar_stat(nHRU,nFreq),stat=err)
 if (err/=0) then
  err=20
  message=trim(message)//"problemAllocateDataTopLevel-Mvar"
  return
 endif

 ! forcing variables
 if(associated(forc_stat)) deallocate(mvar_stat)
 allocate(forc_stat(nHRU,nFreq),stat=err)
 if (err/=0) then
  err=20
  message=trim(message)//"problemAllocateDataTopLevel-Forc"
  return
 endif

 ! basin variables
 if(associated(bvar_stat)) deallocate(bvar_stat)
 allocate(bvar_stat(nFreq),stat=err)
 if (err/=0) then
  err=20
  message=trim(message)//"problemAllocateDataTopLevel-Bvar"
  return
 endif

 ! integrated variables
 if(associated(intg_stat)) deallocate(intg_stat)
 allocate(intg_stat(nHRU,nFreq,nLayers),stat=err)
 if (err/=0) then
  err=20
  message=trim(message)//"problemAllocateDataTopLevel-Intg"
  return
 endif

 ! --------------------------------------------
 ! (2) allocate the variable level structure
 ! --------------------------------------------
 ! model variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   allocate(mvar_stat(iHRU,iFreq)%var(maxVarMvar),stat=err)
   if (err/=0) then
    err=20
    message=trim(message)//"problemAllocateData2ndLevel-Mvar"
    return
   endif
  end do ! iFreq
 end do ! iHRU

 ! forcing variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   allocate(forc_stat(iHRU,iFreq)%var(maxVarForc),stat=err)
   if (err/=0) then
    err=20
    message=trim(message)//"problemAllocateData2ndLevel-Forc"
    return
   endif
  end do ! iFreq
 end do ! iHRU

 ! basin variables 
 do iFreq = 1,nFreq
  allocate(bvar_stat(iFreq)%var(maxVarBvar),stat=err)
  if (err/=0) then
   err=20
   message=trim(message)//"problemAllocateData2ndLevel-Bvar"
   return
  endif
 end do ! iFreq

 ! vertically integrated variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   do iLay = 1,nLayers
    allocate(intg_stat(iHRU,iFreq,iLay)%var(maxVarIntg),stat=err)
    if (err/=0) then
     err=20
     message=trim(message)//"problemAllocateData2ndLevel-Intg"
     return
    endif
   end do ! iLay 
  end do ! iFreq 
 end do ! iHRU

 ! --------------------------------------------
 ! (3) allocate the data level structure
 ! --------------------------------------------
 ! model variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   do iVar = 1,maxVarMvar
    allocate(mvar_stat(iHRU,iFreq)%var(iVar)%dat(maxVarStat+1),stat=err)
    if (err/=0) then
     err=20
     message=trim(message)//"problemAllocateData3rdLevel-Mvar"
     return
    endif
   end do ! iVar
  end do ! iFreq
 end do ! iHRU

 ! forcing variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   do iVar = 1,maxVarForc
    allocate(forc_stat(iHRU,iFreq)%var(iVar)%dat(maxVarStat+1),stat=err)
    if (err/=0) then
     err=20
     message=trim(message)//"problemAllocateData3rdLevel-Forc"
     return
    endif
   end do ! iVar
  end do ! iFreq
 end do ! iHRU

 ! basin variables 
  do iFreq = 1,nFreq
   do iVar = 1,maxVarBvar
    allocate(bvar_stat(iFreq)%var(iVar)%dat(maxVarStat+1),stat=err)
    if (err/=0) then
     err=20
     message=trim(message)//"problemAllocateData3rdLevel-Bvar"
     return
    endif
   end do ! iVar
  end do ! iFreq

 ! vertically integrated variables 
 do iHRU = 1,nHRU
  do iFreq = 1,nFreq
   do iLay = 1,nLayers
    do iVar = 1,maxVarIntg
     allocate(intg_stat(iHRU,iFreq,iLay)%var(iVar)%dat(maxVarStat+1),stat=err)
     if (err/=0) then
      err=20
      message=trim(message)//"problemAllocateData3rdLevel-Intg"
      return
     endif
    end do ! iVar
   end do ! iLay 
  end do ! iFreq 
 end do ! iHRU

 return
 end subroutine alloc_stats

 ! ******************************************************************************************************
 ! public subroutine compile_stats is called at every model timestep to update/store output statistics 
 ! from model variables
 ! ******************************************************************************************************
 subroutine compile_stats(iHRU,iStep,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only:forc_meta    ! metadata structure for forcing 
 USE data_struc, only:forc_data    ! timestep data structure for forcing
 USE data_struc, only:forc_stat    ! stat data structure for forcing
 USE data_struc, only:mvar_meta    ! metadata structure for local model variable
 USE data_struc, only:mvar_data    ! timestep data structure for local model variable
 USE data_struc, only:mvar_stat    ! stat data structure for local model variable
 USE data_struc, only:intg_meta    ! metadata structure for vertically integrated variable
 USE data_struc, only:intg_stat    ! stat data structure for vertically integrated variable
 USE data_struc, only:indx_data    ! need the number of soil layers from here
 USE data_struc, only:nFreq        ! output frequencies
 USE data_struc, only:ix_soil,ix_snow ! named variable to identify a soil or snow layer
 ! lookup structures
 USE var_lookup, only:maxVarForc   ! number of forcing variables
 USE var_lookup, only:maxVarMvar   ! number of model variables
 USE var_lookup, only:maxVarIntg   ! number of vertically integrated variables
 USE var_lookup, only:iLookVarType ! named variables for type indexes 
 USE var_lookup, only:iLookMvar    ! model variable indexes 
 USE var_lookup, only:iLookIndex   ! model variable indexes 
 USE multiconst, only:integerMissing
 implicit none

 ! dummy variables
 integer(i4b),intent(out)       :: err              ! error code
 character(*),intent(out)       :: message          ! error message
 integer(i4b),intent(in)        :: iStep            ! timestep index to compare with oFreq of each variable
 integer(i4b),intent(in)        :: iHRU             ! HRU index

 ! internals
 character(256)                 :: cmessage         ! error message
 real(dp)                       :: tdata            ! temporary storage of timestep data
 integer(i4b)                   :: iVar             ! index for varaiable loop
 integer(i4b)                   :: iLay             ! index for layer loop
 integer(i4b)                   :: iFreq            ! index for frequency loop
 integer(i4b)                   :: nSoil,nSnow      ! number of layers in snow/soil column

 ! initialize error control
 err=0; message='compile_stats/'

 ! pull number of layers
 nSnow = count(indx_data%var(iLookINDEX%layerType)%dat==ix_snow)  ! number of soil layers
 nSoil = count(indx_data%var(iLookINDEX%layerType)%dat==ix_soil)  ! number of soil layers

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 ! loop through output frequencies 
 do iFreq = 1,nFreq

  ! model variables
  do iVar = 1,maxVarMvar
   tdata = mvar_data%var(iVar)%dat(1)
   call calc_stats(mvar_meta(iVar),mvar_stat(iHRU,iFreq)%var(iVar),tdata,iFreq,iStep,err,cmessage)  
   if(err/=0)then; message=trim(message)//trim(cmessage)//"Mvar";return; endif  
  enddo

  ! forcing variables
  do iVar = 1,maxVarForc 
   tdata = forc_data%var(iVar) 
   call calc_stats(forc_meta(iVar),forc_stat(iHRU,iFreq)%var(iVar),tdata,iFreq,iStep,err,cmessage) 
   if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif      
  enddo

  ! vertically integrated variables
  do iVar = 1,maxVarIntg 
   if (intg_meta(iVar)%mvarID.eq.integerMissing) cycle
   ! take different action depending on whether this is a snow, soil, or toto variable
   select case (mvar_meta(intg_meta(iVar)%mvarID)%varType)
    ! all midpoints
    case (iLookVarType%midToto)
     if (nSnow.gt.0) then
      tdata = sum(mvar_data%var(intg_meta(iVar)%mVarID)%dat(0:nSnow-1))/nSnow
     else
      tdata = 0
     endif
     call calc_stats(intg_meta(iVar),intg_stat(iHRU,iFreq,1)%var(iVar),tdata,iFreq,iStep,err,cmessage)
     if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif         
     do iLay = 1,nSoil
      tdata = mvar_data%var(intg_meta(iVar)%mvarID)%dat(nSnow-1+iLay)
      call calc_stats(intg_meta(iVar),intg_stat(iHRU,iFreq,1+iLay)%var(iVar),tdata,iFreq,iStep,err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif         
     enddo

    ! midpoint in snow only
    case (iLookVarType%midSnow)
     if (nSnow.gt.0) then
      tdata = sum(mvar_data%var(intg_meta(iVar)%mvarID)%dat(:))/nSnow
     else
      tdata = 0
     endif
     call calc_stats(intg_meta(iVar),intg_stat(iHRU,iFreq,1)%var(iVar),tdata,iFreq,iStep,err,cmessage)
     if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif         

    ! midpoint in soil only
    case (iLookVarType%midSoil)
     do iLay = 1,nSoil
      tdata = mvar_data%var(intg_meta(iVar)%mvarID)%dat(iLay)
      call calc_stats(intg_meta(iVar),intg_stat(iHRU,iFreq,1+iLay)%var(iVar),tdata,iFreq,iStep,err,cmessage)
      if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif         
     enddo
   endselect ! midSoil, midSnow, midToto
  enddo ! loop through maxVarIntg

 enddo ! iFreq

 return
 end subroutine compile_stats

 ! ******************************************************************************************************
 ! public subroutine compile_basin_stats is called at every model timestep to update/store output statistics 
 ! from basin-scale variables
 ! ******************************************************************************************************
 subroutine compile_basin_stats(iStep,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only:bvar_meta    ! metadata structure for basin vars 
 USE data_struc, only:bvar_data    ! timeste data structure for basin vars
 USE data_struc, only:bvar_stat    ! stat data structure for basin vars
 USE data_struc, only:nFreq        ! number of output frequencies
 ! lookup structures
 USE var_lookup, only:maxVarBvar   ! number of basin variables
 ! structures of named variables
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err         ! error code
 character(*),intent(out)       :: message     ! error message
 integer(i4b),intent(in)        :: iStep       ! timestep index to compare with outputFreq of each variable
 ! internals
 character(256)                 :: cmessage    ! error message
 real(dp)                       :: tdata       ! temporary storage of timestep data
 integer(i4b)                   :: iVar        ! index for varaiable loop
 integer(i4b)                   :: iFreq       ! index for frequency loop

 ! initialize error control
 err=0; message='compile_basin_stats/'

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 do iFreq = 1,nFreq
  do iVar = 1,maxVarBvar
   tdata = bvar_data%var(iVar)%dat(1)   
   call calc_stats(bvar_meta(iVar),bvar_stat(iFreq)%var(iVar),tdata,iFreq,iStep,err,cmessage)  
   if(err/=0)then; message=trim(message)//trim(cmessage);return; endif     
  enddo
 enddo

 return
 end subroutine compile_basin_stats


 ! ******************************************************************************************************
 ! Private subroutine calc_stats is a generic fucntion to deal with any variable type.
 ! Called from compile_stats 
 ! ******************************************************************************************************
 subroutine calc_stats(meta,stat,tdata,iFreq,iStep,err,message)
 USE nrtype
 ! data structures
 USE data_struc,only:var_info        ! type dec for meta data structures 
 USE data_struc,only:dlength         ! type dec for stats structures 
 USE data_struc,only:maxVarStat      ! # of output statistics 
 USE data_struc,only:outFreq         ! output frequencies 
 ! structures of named variables
 USE var_lookup,only:iLookVarType    ! named variables for variable types 
 USE var_lookup,only:iLookStat       ! named variables for output statistics types 
 implicit none
 ! dummy variables
 class(var_info),intent(in)        :: meta        ! meta dat a structure
 type(dlength)  ,intent(inout)     :: stat        ! statistics structure
 real(dp)       ,intent(in)        :: tdata       ! data structure
 integer(i4b)   ,intent(in)        :: iFreq       ! frequency index
 integer(i4b)   ,intent(in)        :: iStep       ! timestep
 integer(i4b)   ,intent(out)       :: err         ! error code
 character(*)   ,intent(out)       :: message     ! error message
 ! internals
 integer(i4b)                      :: iStat       ! statistics loop
 ! initialize error control
 err=0; message='calc_stats/'

 ! ---------------------------------------------
 ! reset statistics at new frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outFreq(iFreq)).eq.1) then
  do iStat = 1,maxVarStat                          ! loop through output statistics
   if (.not.meta%statFlg(iFreq,iStat)) cycle       ! don't bother if output flag is off
   if (meta%varType.ne.iLookVarType%scalarv) cycle ! only calculate stats for scalars 
   select case(iStat)                              ! act depending on the statistic 
    case (iLookStat%totl)                          ! summation over period
     stat%dat(iStat) = 0                           ! resets stat at beginning of period
    case (iLookStat%mean)                          ! mean over period
     stat%dat(iStat) = 0. 
    case (iLookStat%vari)                          ! variance over period
     stat%dat(iStat) = 0                           ! resets E[X^2] term in var calc
     stat%dat(maxVarStat+1) = 0                    ! resets E[X]^2 term  
    case (iLookStat%mini)                          ! minimum over period
     stat%dat(iStat) = huge(stat%dat(iStat))       ! resets stat at beginning of period
    case (iLookStat%maxi)                          ! maximum over period
     stat%dat(iStat) = -huge(stat%dat(iStat))      ! resets stat at beginning of period
    case (iLookStat%mode)                          ! mode over period (does not work)
     stat%dat(iStat) = -9999.
   endselect
  enddo ! iStat 
 endif

 ! ---------------------------------------------
 ! Calculate each statistic that is requested by user
 ! ---------------------------------------------
 do iStat = 1,maxVarStat                           ! loop through output statistics
  if (.not.meta%statFlg(iFreq,iStat)) cycle        ! do not bother if output flag is off
  if (meta%varType.ne.iLookVarType%scalarv) cycle  ! only calculate stats for scalars 
  select case(iStat)                               ! act depending on the statistic 
   case (iLookStat%totl)                           ! summation over period
    stat%dat(iStat) = stat%dat(iStat) + tdata      ! into summation
   case (iLookStat%inst)                           ! instantaneous
    stat%dat(iStat) = tdata                                        
   case (iLookStat%mean)                           ! mean over period
    stat%dat(iStat) = stat%dat(iStat) + tdata      ! adds timestep to sum 
   case (iLookStat%vari)                           ! variance over period
    stat%dat(iStat) = stat%dat(iStat) + tdata**2   ! sum into E[X^2] term
    stat%dat(maxVarStat+1) = stat%dat(maxVarStat+1) + tdata  ! sum into E[X]^2 term        
   case (iLookStat%mini)                           ! minimum over period
    if (tdata.le.stat%dat(iStat)) stat%dat(iStat) = tdata ! overwrites minimum iff 
   case (iLookStat%maxi)                           ! maximum over period
    if (tdata.ge.stat%dat(iStat)) stat%dat(iStat) = tdata ! overwrites maximum iff 
   case (iLookStat%mode)                           ! (does not work)
    stat%dat(iStat) = -9999. 
  endselect
 enddo ! iStat 

 ! ---------------------------------------------
 ! finalize statistics at end of frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outFreq(iFreq)).eq.0) then
  do iStat = 1,maxVarStat                          ! loop through output statistics
   if (.not.meta%statFlg(iFreq,iStat)) cycle       ! do not bother if output flag is off
   if (meta%vartype.ne.iLookVarType%scalarv) cycle ! only calculate stats for scalars 
   select case(iStat)                              ! act depending on the statistic 
    case (iLookStat%mean)                          ! mean over period
     stat%dat(iStat) = stat%dat(iStat)/outFreq(iFreq) ! normalize sum into mean
    case (iLookStat%vari)                          ! variance over period
     stat%dat(maxVarStat+1) = stat%dat(maxVarStat+1)/outFreq(iFreq) ! E[X] term
     stat%dat(iStat) = stat%dat(iStat)/outFreq(iFreq) + stat%dat(maxVarStat+1) ! full variance
   endselect
  enddo ! iStat 
 endif

 return
 end subroutine calc_stats

end module module_output
