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
 USE var_lookup, only: maxvarMvar               ! # of dynamic model variables 
 USE var_lookup, only: maxvarForc               ! # of forcing variables 
 USE var_lookup, only: maxvarBvar               ! # of basin variables 
 USE var_lookup, only: maxvarStat               ! # of output statistics 
 USE var_lookup, only: maxvarIntg               ! # of integrated variables 
 USE data_struc, only: maxIntLayr               ! # of integration layers
 USE data_struc, only: indx_data                ! timestep-specific layer index data 
 ! structures of named variables
 USE var_lookup, only: iLookIndex               ! named variables for layer indices 
! USE var_lookup, only: iLookVarType             ! to index into variable type 
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
 integer(i4b)                   :: iInt                           ! loop through vertically integrated layers
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
 ! structures of named variables
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err                            ! error code
 character(*),intent(out)       :: message                        ! error message
 integer(i4b),intent(in)        :: istep                          ! timestep index to compare with outputFreq of each variable
 integer(i4b),intent(in)        :: iHRU                           ! HRU index
 ! internals
 character(256)                 :: cmessage                       ! error message
 real(dp),allocatable           :: tdata(:)                       ! temporary storage of timestep data
 integer(i4b)                   :: iVar                           ! index for varaiable loop
 integer(i4b)                   :: iInt                           ! index for integrted layer loop

 ! initialize error control
 err=0; message='compile_stats/'

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 ! model variables
 allocate(tdata(maxvarMvar))                                                           ! allocate structrue to hold the timestep data
 do iVar = 1,maxvarMvar; tdata(iVar) = mvar_data%var(iVar)%dat(1); enddo               ! store timestep data in structure
 call calc_stats(mvar_meta,mvar_stat(iHRU),tdata,maxvarMvar,istep,err,cmessage)        ! calculate the model variable stats 
 if(err/=0)then; message=trim(message)//trim(cmessage)//"Mvar";return; endif           ! error handling
 deallocate(tdata)                                                                     ! deallocate for next data structure 
 
 ! forcing variables
 allocate(tdata(maxvarForc))                                                           ! allocate structrue to hold the timestep data
 do iVar = 1,maxvarForc; tdata(iVar) = forc_data%var(iVar); enddo                      ! store timestep data in structure
 call calc_stats(forc_meta,forc_stat(iHRU),tdata,maxvarForc,istep,err,cmessage)        ! calculate the forcing variable stats 
 if(err/=0)then; message=trim(message)//trim(cmessage)//"Forc";return; endif           ! error handling
 deallocate(tdata)                                                                     ! deallocate for next data structure 

 ! vertically integrated variables
 do iInt = 1,maxIntLayr                                                                ! loop through integration layers
  allocate(tdata(maxvarIntg))                                                          ! allocate structrue to hold the timestep data
  do iVar = 1,maxvarIntg                                                               ! loop through integrated variables
   select case(intg_meta(iVar,iInt)%layertype)
    case (iLookVarType%midToto)
     tdata(iVar) = sum(mvar_data%var(iVar)%dat(:))
    case (iLookVarType%midSnow)
     tdata(iVar) = sum(mvar_data%var(iVar)%dat(:))
    case (iLookVarType%midSoil)
     tdata(iVar) = sum(mvar_data%var(iVar)%dat(:))
    case default
     err=20; message=trim(message)//"variable type not found for integration";return; 
   endselect   
  enddo ! variable in structure            
  call calc_stats(intg_meta,intg_stat(iHRU,iInt),tdata,maxvarIntg,istep,err,cmessage)  ! calculate the integrated variable stats 
  if(err/=0)then; message=trim(message)//trim(cmessage)//"Intg";return; endif          ! error handling
  deallocate(tdata)                                                                    ! deallocate for next data structure 
 enddo ! integration layer                     

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
 real(dp),allocatable           :: tdata(:)    ! temporary storage of timestep data
 integer(i4b)                   :: iVar        ! index for varaiable loop

 ! initialize error control
 err=0; message='compile_basin_stats/'

 ! --------------------------------------------
 ! Calculate timestep statistics 
 ! --------------------------------------------
 allocate(tdata(maxvarBvar))                                                           ! allocate structrue to hold the timestep data
 do iVar = 1,maxvarBvar; tdata(iVar) = bvar_data%var(iVar)%dat(1); enddo               ! store timestep data in structure
 call calc_stats(bvar_meta,bvar_stat,tdata,maxvarBvar,istep,err,cmessage)              ! calculate the forcing variable stats 
 if(err/=0)then; message=trim(message)//trim(cmessage);return; endif                   ! error handling
 deallocate(tdata)                                                                     ! deallocate for next data structure 

 return
 end subroutine compile_basin_stats


 ! ******************************************************************************************************
 ! Private subroutine calc_stats is a generic fucntion to deal with any variable type.
 ! Called from compile_stats 
 ! ******************************************************************************************************
 subroutine calc_stats(meta,stat,tdata,nvar,istep,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only:var_info        ! type dec for passed meta structure 
 USE data_struc, only:var_dlength     ! type dec for passed data & stats structures 
 USE data_struc, only:outputFrequency ! output frequency
 ! structures of named variables
 USE var_lookup, only:iLookVarType    ! named variables for variable types 
 USE var_lookup, only:iLookStat       ! named variables for output statistics types 
 USE var_lookup, only:maxvarStat      ! # of output statistics 
 implicit none
 ! dummy variables
 integer(i4b),intent(out)                  :: err         ! error code
 character(*),intent(out)                  :: message     ! error message
 integer(i4b),intent(in)                   :: nvar        ! number of variables in structure
 integer(i4b),intent(in)                   :: istep       ! timestep
 type(var_info),intent(in)                 :: meta(nvar)  ! metadata structure
 real(dp),intent(in)                       :: tdata(nvar) ! data structure
 type(var_dlength),intent(inout)           :: stat  ! statistics structure
 ! internals
 integer(i4b)                              :: iVar     ! variable loop
 integer(i4b)                              :: iStat    ! statistics loop
 ! initialize error control
 err=0; message='calc_stats/'

 ! ---------------------------------------------
 ! reset statistics at new frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outputFrequency).eq.1) then
  do iVar=1,nvar                                                    ! loop through variables in structure
   do iStat=1,maxvarStat                                            ! loop through output statistics
    if (.not.meta(iVar)%stat(iStat)) cycle                          ! do not bother if output flag is off
    if (meta(iVar)%vartype.ne.iLookVarType%scalarv) cycle           ! only calculate stats for scalar vaiables 
    select case(iStat)                                              ! act depending on the statistic 
     case (iLookStat%mean)                                          ! mean over period
      stat%var(iVar)%dat(iStat) = 0. 
     case (iLookStat%vari)                                          ! variance over period
       stat%var(iVar)%dat(iStat) = 0                                ! resets E[X^2] term in variance calculation
       stat%var(iVar)%dat(maxvarStat+1) = 0                         ! resets E[X]^2 term  
     case (iLookStat%mini)                                          ! minimum over period
      stat%var(iVar)%dat(iStat) = huge(stat%var(iVar)%dat(iStat))   ! resets stat at beginning of period
     case (iLookStat%maxi)                                          ! maximum over period
      stat%var(iVar)%dat(iStat) = -huge(stat%var(iVar)%dat(iStat))  ! resets stat at beginning of period
     case (iLookStat%mode)                                          ! mode over period (does not work)
      stat%var(iVar)%dat(iStat) = -9999.
     case (iLookStat%harm)                                          ! harmonic mean over period
      stat%var(iVar)%dat(iStat) = 0                                 ! resets stat at beginning of period
     case (iLookStat%geom)                                          ! geometric mean over period
      stat%var(iVar)%dat(iStat) = 0                                 ! resets stat at beginning of period
     case (iLookStat%totl)                                          ! summation over period
      stat%var(iVar)%dat(iStat) = 0                                 ! resets stat at beginning of period
    endselect
   enddo ! iStat 
  enddo ! variable 
 endif

 ! ---------------------------------------------
 ! Calculate each statistic that is requested by user
 ! ---------------------------------------------
 do iVar=1,nvar                                                     ! loop through variables in structure
  do iStat=1,maxvarStat                                             ! loop through output statistics
   if (.not.meta(iVar)%stat(iStat)) cycle                           ! do not bother if output flag is off
   if (meta(iVar)%vartype.ne.iLookVarType%scalarv) cycle            ! only calculate stats for scalar vaiables 

   ! act depending on the statistic 
   select case(iStat)
    case (iLookStat%inst) ! instantaneous
     stat%var(iVar)%dat(iStat) = tdata(iVar)                                        
    case (iLookStat%mean) ! mean over period
     stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat) + tdata(iVar)                    ! adds timestep to sum 
    case (iLookStat%vari) ! variance over period
     stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat) + tdata(iVar)**2                 ! sum into E[X^2] term
     stat%var(iVar)%dat(maxvarStat+1) = stat%var(iVar)%dat(maxvarStat+1) + tdata(iVar)      ! sum into E[X]^2 term             
    case (iLookStat%mini) ! minimum over period
     if (tdata(iVar).le.stat%var(iVar)%dat(iStat)) stat%var(iVar)%dat(iStat) = tdata(iVar)  ! overwrites minimum if warranted
    case (iLookStat%maxi) ! maximum over period
     if (tdata(iVar).ge.stat%var(iVar)%dat(iStat)) stat%var(iVar)%dat(iStat) = tdata(iVar)  ! overwrites maximum if warranted
    case (iLookStat%mode) ! mode over period (does not work)
     stat%var(iVar)%dat(iStat) = -9999. 
    case (iLookStat%harm) ! harmonic mean over period
     stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat) + 1/tdata(iVar)                  ! into summation
    case (iLookStat%geom) ! geometric mean over period
     stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat) * tdata(iVar)                    ! into summation
    case (iLookStat%totl)                                                                   ! summation over period
     stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat) + tdata(iVar)                    ! into summation
   endselect
  enddo ! iStat 
 enddo ! iVar

 ! ---------------------------------------------
 ! finalize statistics at end of frequenncy period 
 ! ---------------------------------------------
 if (mod(iStep,outputFrequency).eq.0) then
  do iVar=1,nvar                                                                                                         ! loop through variables in structure
   do iStat=1,maxvarStat                                                                                                 ! loop through output statistics
    if (.not.meta(iVar)%stat(iStat)) cycle                                                                               ! do not bother if output flag is off
    if (meta(iVar)%vartype.ne.iLookVarType%scalarv) cycle                                                                ! only calculate stats for scalar vaiables 
    select case(iStat)                                                                                                   ! act depending on the statistic 
     case (iLookStat%mean)                                                                                               ! mean over period
      stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat)/outputFrequency                                              ! normalize sum into mean at end of period 
     case (iLookStat%vari)                                                                                               ! variance over period
      stat%var(ivar)%dat(maxvarStat+1) = stat%var(iVar)%dat(maxvarStat+1)/outputFrequency                                ! E[X] term
      stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat)/outputFrequency + stat%var(iVar)%dat(maxvarStat+1)           ! full variance
     case (iLookStat%harm)                                                                                               ! harmonic mean over period
      stat%var(iVar)%dat(iStat) = 1/stat%var(iVar)%dat(iStat) * outputFrequency                                          ! normalize at end of period
     case (iLookStat%geom)                                                                                               ! geometric mean over period
      stat%var(iVar)%dat(iStat) = stat%var(iVar)%dat(iStat)**(1/outputFrequency)                                         ! normalize at end of period
    endselect
   enddo ! iStat 
  enddo ! variable 
 endif

 return
 end subroutine calc_stats
end module module_output
