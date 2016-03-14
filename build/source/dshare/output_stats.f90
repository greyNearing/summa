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
 USE data_struc, only: mvar_stat                ! (to be allocated) stat data structure for local model variables
 USE data_struc, only: forc_stat                ! (to be allocated) stat data structure for forcing variables
 USE data_struc, only: bvar_stat                ! (to be allocated) stat data structure for local basin variables
 USE data_struc, only: intg_stat                ! (to be allocated) stat data structure for vertically integrated variables
 USE data_struc, only: maxvarStat               ! # of output statistics 
 USE var_lookup, only: maxvarMvar               ! # of dynamic model variables 
 USE var_lookup, only: maxvarForc               ! # of forcing variables 
 USE var_lookup, only: maxvarBvar               ! # of basin variables 
 USE var_lookup, only: maxvarIntg               ! # of integrated variables 
 USE data_struc, only: maxIntLayr               ! # of integration layers
 USE data_struc, only: indx_data                ! timestep-specific layer index data 
 ! structures of named variables
 USE var_lookup, only: iLookIndex               ! named variables for layer indices 
! USE var_lookup, only: iLookVarType            ! to index into variable type 
 ! index into various data types 
 USE get_ixname_module, only: get_varTypeName   ! get variable type name for error message
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err                            ! error code
 character(*),intent(out)       :: message                        ! error message
 integer(i4b),intent(in)        :: nHRU                           ! number of HRUs
 ! local variables
 integer(i4b)                   :: iVar                           ! index var_info array 
 integer(i4b)                   :: iStat                          ! index var_stat array 
 integer(i4b)                   :: iInt                           ! index integration layers 
 real(dp),parameter             :: missingDouble=-9999._dp
 integer(i4b),parameter         :: nBand=2                        ! ?why is this not a part of var_lookup?
 integer(i4b)                   :: iHRU                           ! loop through HRUs
 integer(i4b)                   :: nVar                           ! number of variables
 ! initialize error control
 err=0; message='alloc_stats/'

 ! --------------------------------------------
 ! (1) allocate the HRU level structure
 ! --------------------------------------------
 ! model variables
 if(associated(mvar_stat)) deallocate(mvar_stat)
 allocate(mvar_stat(nHRU),stat=err)
 if(err/=0)then; err=20; message=trim(message)//"problemAllocateDataTopLevel-Mvar"; return; endif
 ! forcing variables
 if(associated(forc_stat)) deallocate(mvar_stat)
 allocate(forc_stat(nHRU),stat=err)
 if(err/=0)then; err=20; message=trim(message)//"problemAllocateDataTopLevel-Forc"; return; endif
 ! basin variables
 if(associated(bvar_stat)) deallocate(bvar_stat)
 allocate(bvar_stat,stat=err)
 if(err/=0)then; err=20; message=trim(message)//"problemAllocateDataTopLevel-Bvar"; return; endif
 ! integrated variables
 if(associated(intg_stat)) deallocate(intg_stat)
 allocate(intg_stat(nHRU,maxIntLayr),stat=err)
 if(err/=0)then; err=20; message=trim(message)//"problemAllocateDataTopLevel-Intg"; return; endif

 ! --------------------------------------------
 ! (2) allocate the variable level structure
 ! --------------------------------------------
 ! model variables 
 do iHRU=1,nHRU
  allocate(mvar_stat(iHRU)%var(maxvarMvar),stat=err)
  if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Mvar"; return; endif
 end do ! (looping through the HRUs)
 ! forcing variables 
 do iHRU=1,nHRU
  allocate(forc_stat(iHRU)%var(maxvarForc),stat=err)
  if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Forc"; return; endif
 end do ! (looping through the HRUs)
 ! basin variables 
 allocate(bvar_stat%var(maxvarBvar),stat=err)
 if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Bvar"; return; endif
 ! vertically integrated variables 
 do iHRU=1,nHRU
  do iInt = 1,maxIntLayr
   allocate(intg_stat(iHRU,iInt)%var(maxvarIntg),stat=err)
   if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Intg"; return; endif
  enddo ! integration layers
 end do ! (looping through the HRUs)

 ! --------------------------------------------
 ! (3) allocate the data level structure
 ! --------------------------------------------
 ! model variables 
 do iHRU=1,nHRU
  do iVar = 1,maxvarMvar
   allocate(mvar_stat(iHRU)%var(iVar)%dat(maxvarStat+1),stat=err)
   if(err/=0)then; err=20; message=trim(message)//"problemAllocateData3rdLevel-Mvar"; return; endif
  end do ! model variable
 end do ! (looping through the HRUs)
 ! forcing variables 
 do iHRU=1,nHRU
  do iVar = 1,maxvarForc
   allocate(forc_stat(iHRU)%var(iVar)%dat(maxvarStat+1),stat=err)
   if(err/=0)then; err=20; message=trim(message)//"problemAllocateData3rdLevel-Forc"; return; endif
  end do ! forcing variables
 end do ! (looping through the HRUs)
 ! basin variables 
 do iVar = 1,maxvarBvar
  allocate(bvar_stat%var(iVar)%dat(maxvarStat+1),stat=err)
  if(err/=0)then; err=20; message=trim(message)//"problemAllocateData3rdLevel-Bvar"; return; endif
 end do ! basin variables
 ! vertically integrated variables 
 do iHRU=1,nHRU
  do iVar = 1,maxvarIntg
   do iInt = 1,maxIntLayr
    allocate(intg_stat(iHRU,iInt)%var(iVar)%dat(maxvarStat+1),stat=err)
    if(err/=0)then; err=20; message=trim(message)//"problemAllocateData3rdLevel-Forc"; return; endif
   end do ! integration layers
  end do ! integrtaed variables
 end do ! (looping through the HRUs)

 return
 end subroutine alloc_stats

 ! ******************************************************************************************************
 ! public subroutine compile_stats is called at every model timestep to update/store output statistics 
 ! from model variables
 ! ******************************************************************************************************
 subroutine compile_stats(iHRU,istep,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only:forc_meta    ! metadata structure for forcing 
 USE data_struc, only:forc_data    ! timeste data structure for forcing
 USE data_struc, only:forc_stat    ! stat data structure for forcing
 USE data_struc, only:mvar_meta    ! metadata structure for local model variable
 USE data_struc, only:mvar_data    ! timeste data structure for local model variable
 USE data_struc, only:mvar_stat    ! stat data structure for local model variable
 USE data_struc, only:intg_meta    ! metadata structure for vertically integrated variable
 USE data_struc, only:intg_stat    ! stat data structure for vertically integrated variable
 ! lookup structures
 USE var_lookup, only:maxvarForc   ! number of forcing variables
 USE var_lookup, only:maxvarMvar   ! number of model variables
 USE var_lookup, only:maxvarIntg   ! number of vertically integrated variables
 USE data_struc, only:maxIntLayr   ! number of vertically integrated layers
 USE var_lookup, only:iLookVarType ! named variables for type indexes 
 ! structures of named variables
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err                            ! error code
 character(*),intent(out)       :: message                        ! error message
 integer(i4b),intent(in)        :: istep                          ! timestep index to compare with outputFreq of each variable
 integer(i4b),intent(in)        :: iHRU                           ! HRU index
 ! internals
 character(256)                 :: cmessage                       ! error message
 real(dp)                       :: tdata                          ! temporary storage of timestep data
 integer(i4b)                   :: iVar                           ! index for varaiable loop
 integer(i4b)                   :: iInt                           ! index for integrted layer loop

 ! initialize error control
 err=0; message='compile_stats/'

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 ! model variables
 do iVar = 1,maxvarMvar                                                                ! loop through variabels in structure
  tdata = mvar_data%var(iVar)%dat(1)
  call calc_stats(mvar_meta(iVar)%stat,mvar_meta(iVar)%vartype,mvar_stat(iHRU)%var(iVar),tdata,istep,err,cmessage)  ! calculate the model variable stats 
  if(err/=0)then; message=trim(message)//trim(cmessage)//"Mvar";return; endif          ! error handling
 enddo
 
 ! forcing variables
 do iVar = 1,maxvarForc                                                                ! loop through vars in structure
  tdata = forc_data%var(iVar) 
  call calc_stats(forc_meta(iVar)%stat,forc_meta(iVar)%vartype,forc_stat(iHRU)%var(iVar),tdata,istep,err,cmessage)  ! calculate the forcing variable stats 
  if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif          ! error handling
 enddo

 ! vertically integrated variables
 do iVar = 1,maxvarIntg                                                                ! loop through integrated variables
  do iInt = 1,maxIntLayr                                                               ! loop through integration layers
   if (intg_meta(iVar,iInt)%mVarID.le.0) cycle                                         ! skip if this variable is not used for any statistic
   select case(intg_meta(iVar,iInt)%layertype)
    case (iLookVarType%midToto)
     tdata = sum(mvar_data%var(intg_meta(iVar,iInt)%mVarID)%dat(:))
    case (iLookVarType%midSnow)
     tdata = sum(mvar_data%var(intg_meta(iVar,iInt)%mVarID)%dat(:))
    case (iLookVarType%midSoil)
     tdata = sum(mvar_data%var(intg_meta(iVar,iInt)%mVarID)%dat(:))
    case default
     err=20; message=trim(message)//"variable type not found for integration";return; 
   endselect   
   call calc_stats(intg_meta(iVar,iInt)%stat,intg_meta(iVar,iInt)%vartype,intg_stat(iHRU,iInt)%var(iVar),tdata,istep,err,cmessage)  ! calculate the integrated variable stats 
   if(err/=0)then; message=trim(message)//trim(cmessage)//"Intg";return; endif          ! error handling
  enddo ! integration layer                     
 enddo ! variable in structure            

 return
 end subroutine compile_stats

 ! ******************************************************************************************************
 ! public subroutine compile_basin_stats is called at every model timestep to update/store output statistics 
 ! from basin-scale variables
 ! ******************************************************************************************************
 subroutine compile_basin_stats(istep,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only:bvar_meta    ! metadata structure for basin vars 
 USE data_struc, only:bvar_data    ! timeste data structure for basin vars
 USE data_struc, only:bvar_stat    ! stat data structure for basin vars
 ! lookup structures
 USE var_lookup, only:maxvarBvar   ! number of basin variables
 ! structures of named variables
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err         ! error code
 character(*),intent(out)       :: message     ! error message
 integer(i4b),intent(in)        :: istep       ! timestep index to compare with outputFreq of each variable
 ! internals
 character(256)                 :: cmessage    ! error message
 real(dp)                       :: tdata       ! temporary storage of timestep data
 integer(i4b)                   :: iVar        ! index for varaiable loop

 ! initialize error control
 err=0; message='compile_basin_stats/'

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 do iVar = 1,maxvarBvar
  tdata = bvar_data%var(iVar)%dat(1)                                             ! store timestep data in structure
  call calc_stats(bvar_meta(iVar)%stat,bvar_meta(iVar)%vartype,bvar_stat%var(iVar),tdata,istep,err,cmessage)   ! calculate the forcing variable stats 
  if(err/=0)then; message=trim(message)//trim(cmessage);return; endif            ! error handling
 enddo

 return
 end subroutine compile_basin_stats


 ! ******************************************************************************************************
 ! Private subroutine calc_stats is a generic fucntion to deal with any variable type.
 ! Called from compile_stats 
 ! ******************************************************************************************************
 subroutine calc_stats(stat_flag,vartype,stat,tdata,istep,err,message)
 USE nrtype
 ! data structures
 USE data_struc,only:dlength         ! type dec for passed data & stats structures 
 USE data_struc,only:outputFrequency ! output frequency
 USE data_struc,only:maxvarStat      ! # of output statistics 
 ! structures of named variables
 USE var_lookup,only:iLookVarType    ! named variables for variable types 
 USE var_lookup,only:iLookStat       ! named variables for output statistics types 
 implicit none
 ! dummy variables
 integer(i4b),intent(out)                      :: err         ! error code
 character(*),intent(out)                      :: message     ! error message
 integer(i4b),intent(in)                       :: istep       ! timestep
 integer(i4b),intent(in)                       :: vartype     ! variable type
 logical(lgt),dimension(maxVarStat),intent(in) :: stat_flag   ! is stat turned on
 real(dp),intent(in)                           :: tdata       ! data structure
 type(dlength),intent(inout)                   :: stat        ! statistics structure
 ! internals
 integer(i4b)                                  :: iStat       ! statistics loop
 ! initialize error control
 err=0; message='calc_stats/'

 ! ---------------------------------------------
 ! reset statistics at new frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outputFrequency).eq.1) then
  do iStat=1,maxvarStat                              ! loop through output statistics
   if (.not.stat_flag(iStat)) cycle                  ! do not bother if output flag is off
   if (vartype.ne.iLookVarType%scalarv) cycle   ! only calculate stats for scalar vaiables 
   select case(iStat)                                ! act depending on the statistic 
    case (iLookStat%mean)                            ! mean over period
     stat%dat(iStat) = 0. 
    case (iLookStat%vari)                            ! variance over period
     stat%dat(iStat) = 0                             ! resets E[X^2] term in variance calculation
     stat%dat(maxvarStat+1) = 0                      ! resets E[X]^2 term  
    case (iLookStat%mini)                            ! minimum over period
     stat%dat(iStat) = huge(stat%dat(iStat))         ! resets stat at beginning of period
    case (iLookStat%maxi)                            ! maximum over period
     stat%dat(iStat) = -huge(stat%dat(iStat))        ! resets stat at beginning of period
    case (iLookStat%mode)                            ! mode over period (does not work)
     stat%dat(iStat) = -9999.
    case (iLookStat%harm)                            ! harmonic mean over period
     stat%dat(iStat) = 0                             ! resets stat at beginning of period
    case (iLookStat%geom)                            ! geometric mean over period
     stat%dat(iStat) = 0                             ! resets stat at beginning of period
    case (iLookStat%totl)                            ! summation over period
     stat%dat(iStat) = 0                             ! resets stat at beginning of period
   endselect
  enddo ! iStat 
 endif

 ! ---------------------------------------------
 ! Calculate each statistic that is requested by user
 ! ---------------------------------------------
 do iStat=1,maxvarStat                                       ! loop through output statistics
  if (.not.stat_flag(iStat)) cycle                           ! do not bother if output flag is off
  if (vartype.ne.iLookVarType%scalarv) cycle            ! only calculate stats for scalar vaiables 

  ! act depending on the statistic 
  select case(iStat)
   case (iLookStat%inst)                                     ! instantaneous
    stat%dat(iStat) = tdata                                        
   case (iLookStat%mean)                                     ! mean over period
    stat%dat(iStat) = stat%dat(iStat) + tdata                ! adds timestep to sum 
   case (iLookStat%vari)                                     ! variance over period
    stat%dat(iStat) = stat%dat(iStat) + tdata**2             ! sum into E[X^2] term
    stat%dat(maxvarStat+1) = stat%dat(maxvarStat+1) + tdata  ! sum into E[X]^2 term             
   case (iLookStat%mini)                                     ! minimum over period
    if (tdata.le.stat%dat(iStat)) stat%dat(iStat) = tdata    ! overwrites minimum if warranted
   case (iLookStat%maxi)                                     ! maximum over period
    if (tdata.ge.stat%dat(iStat)) stat%dat(iStat) = tdata    ! overwrites maximum if warranted
   case (iLookStat%mode)                                     ! mode over period (does not work)
    stat%dat(iStat) = -9999. 
   case (iLookStat%harm)                                     ! harmonic mean over period
    stat%dat(iStat) = stat%dat(iStat) + 1/tdata              ! into summation
   case (iLookStat%geom)                                     ! geometric mean over period
    stat%dat(iStat) = stat%dat(iStat) * tdata                ! into summation
   case (iLookStat%totl)                                     ! summation over period
    stat%dat(iStat) = stat%dat(iStat) + tdata                ! into summation
  endselect
 enddo ! iStat 

 ! ---------------------------------------------
 ! finalize statistics at end of frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outputFrequency).eq.0) then
  do iStat=1,maxvarStat                                                          ! loop through output statistics
   if (.not.stat_flag(iStat)) cycle                                              ! do not bother if output flag is off
   if (vartype.ne.iLookVarType%scalarv) cycle                               ! only calculate stats for scalar vaiables 
   select case(iStat)                                                            ! act depending on the statistic 
    case (iLookStat%mean)                                                        ! mean over period
     stat%dat(iStat) = stat%dat(iStat)/outputFrequency                           ! normalize sum into mean at end of period 
    case (iLookStat%vari)                                                        ! variance over period
     stat%dat(maxvarStat+1) = stat%dat(maxvarStat+1)/outputFrequency             ! E[X] term
     stat%dat(iStat) = stat%dat(iStat)/outputFrequency + stat%dat(maxvarStat+1)  ! full variance
    case (iLookStat%harm)                                                        ! harmonic mean over period
     stat%dat(iStat) = 1/stat%dat(iStat) * outputFrequency                       ! normalize at end of period
    case (iLookStat%geom)                                                        ! geometric mean over period
     stat%dat(iStat) = stat%dat(iStat)**(1/outputFrequency)                      ! normalize at end of period
   endselect
  enddo ! iStat 
 endif

 return
 end subroutine calc_stats
end module module_output
