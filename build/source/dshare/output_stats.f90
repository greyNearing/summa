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
contains

 ! ******************************************************************************************************
 ! public subroutine alloc_stats is called at beginning of simulation to allocate space for output statistics 
 ! ******************************************************************************************************
 subroutine alloc_stats(nHRU,err,message)
 USE nrtype
 ! data structures
 USE data_struc, only: mvar_stat                ! (to be allocated) stat data structure for local model variables
 USE data_struc, only: mvar_meta                ! model metadata structure 
 USE data_struc, only: forc_stat                ! (to be allocated) stat data structure for forcing variables
 USE data_struc, only: forc_meta                ! forcing metadata structure 
 USE var_lookup, only: maxvarMvar               ! # of dynamic model variables 
 USE var_lookup, only: maxvarForc               ! # of forcing variables 
 USE var_lookup, only: maxvarStats              ! # of output statistics 
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
 integer(i4b)                   :: iVar                           ! index into var_info arrays (for loop)
 integer(i4b)                   :: iStat                          ! index into each dimension of a var_stat array (for loop)
 real(dp),parameter             :: missingDouble=-9999._dp
 integer(i4b),parameter         :: nBand=2                        ! ?why is this not a part of var_lookup?
 integer(i4b)                   :: iHRU                           ! loop through HRUs
 integer(i4b)                   :: nVar                           ! number of variables
 ! initialize error control
 err=0; message='alloc_stats/'

 ! check that the metadata structure is allocated
 if(.not.associated(mvar_meta))then
 err=10; message=trim(message)//"metadataNotInitialized"; return
 endif

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

 ! --------------------------------------------
 ! (2) allocate the variable level structure
 ! --------------------------------------------
 ! model variables 
 do iHRU=1,nHRU
  allocate(mvar_stat(iHRU)%stat(maxvarMvar,maxvarStats+1),stat=err)
  if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Mvar"; return; endif
 end do ! (looping through the HRUs)
 ! forcing variables 
 do iHRU=1,nHRU
  allocate(forc_stat(iHRU)%stat(maxvarForc,maxvarStats+1),stat=err)
  if(err/=0)then; err=20; message=trim(message)//"problemAllocateData2ndLevel-Forc"; return; endif
 end do ! (looping through the HRUs)
 
! ! --------------------------------------------
! ! (3) allocate data level structure
! ! --------------------------------------------
! ! model variables
! do iHRU=1,nHRU
!  do iVar=1,maxvarMvar
!   do iStat=1,maxvarStats+1
!    select case(mvar_meta(ivar)%vartype)
!     case(iLookVarType%scalarv); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(1),stat=err)         ! scalarv
!!     case(iLookVarType%wLength); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(nBand),stat=err)     ! wLength
!!     case(iLookVarType%midSnow); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(nSnow),stat=err)     ! midSnow
!!     case(iLookVarType%midSoil); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(nSoil),stat=err)     ! midSoil
!!     case(iLookVarType%midToto); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(nLayers),stat=err)   ! midToto
!!     case(iLookVarType%ifcSnow); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(0:nSnow),stat=err)   ! ifcSnow
!!     case(iLookVarType%ifcSoil); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(0:nSoil),stat=err)   ! ifcSoil
!!     case(iLookVarType%ifcToto); allocate(mvar_stat(iHRU)%stat(iVar,iStat)%dat(0:nLayers),stat=err) ! ifcToto
!     case default
!      err=41; message=trim(message)//"unknownVariableType[name='"//trim(mvar_meta(ivar)%varname)//"'; &
!                                     &type='"//trim(get_varTypeName(mvar_meta(ivar)%vartype))//"']"; return
!    endselect
!    if(err/=0)then;err=30;message=trim(message)//"problemAllocate[var='"//trim(mvar_meta(ivar)%varname)//"']"; return; endif
!    ! fill data with missing values
!    mvar_stat(iHRU)%stat(iVar,iStat)%dat = missingDouble
!   enddo ! iStat
!  enddo ! iVar
! enddo ! iHRU



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
 ! structures of named variables
 USE var_lookup, only:iLookVarType ! named variables for variable types 
 USE var_lookup, only:iLookStat    ! named variables for output statistics types 
 USE var_lookup, only:maxvarForc   ! # of forcing variables 
 USE var_lookup, only:maxvarMvar   ! # of dynamic model variables 
 USE var_lookup, only:maxvarStats  ! # of output statistics 
 ! index into various data types - fore readability and backward-compatibility
 USE get_ixname_module,only:get_ixForce  ! forcing index function
 USE get_ixname_module,only:get_ixMvar   ! model var index function
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err                            ! error code
 character(*),intent(out)       :: message                        ! error message
 integer(i4b),intent(in)        :: istep                          ! timestep index to compare with o_freq of each variable
 integer(i4b),intent(in)        :: iHRU                           ! HRU index
 ! local variables
 integer(i4b)                   :: iVar                           ! index into var_info arrays (for loop)
 integer(i4b)                   :: iStat                          ! index into each dimension of a var_stat array (for loop)
 ! initialize error control
 err=0; message='compile_stats/'

 ! --------------------------------------------
 ! (1) loop through model variables
 ! --------------------------------------------
  do iVar=1,maxvarMvar                             ! loop through variables in structure
   do iStat=1,maxvarStats                          ! loop through output statistics

    if (mvar_meta(iVar)%o_freq(iStat).le.0) cycle  ! do not bother if output flag is off
    if (mvar_meta(iVar)%vartype.ne.iLookVarType%scalarv) cycle ! only calculate stats for scalar vaiables 

    ! differences between each statistic are (i) algebra, (ii) output frequency
    select case(iStat)
     case (iLookStat%inst) ! instantaneous
      mvar_stat(iHRU)%stat(iVar,iStat) = mvar_data%var(iVar)%dat(1)                                        

     case (iLookStat%mean) ! mean
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) mvar_stat(iHRU)%stat(iVar,iStat) = 0. 
      mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat) + mvar_data%var(iVar)%dat(1) 
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.0) mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat)/mvar_meta(iVar)%o_freq(iStat) 

     case (iLookStat%vari) ! variance
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) then
       mvar_stat(iHRU)%stat(iVar,iStat) = 0 
       mvar_stat(iHRU)%stat(iVar,maxvarStats+1) = 0 
      endif
      mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat) + mvar_data%var(iVar)%dat(1)**2 
      mvar_stat(iHRU)%stat(iVar,maxvarStats+1) = mvar_stat(iHRU)%stat(iVar,maxvarStats+1) + mvar_data%var(iVar)%dat(1)
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.0) then
       mvar_stat(iHRU)%stat(ivar,maxvarStats+1) = mvar_stat(iHRU)%stat(iVar,maxvarStats+1)/mvar_meta(iVar)%o_freq(iStat)
       mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat)/mvar_meta(iVar)%o_freq(iStat) + mvar_stat(iHRU)%stat(iVar,maxvarStats+1) 
      endif

     case (iLookStat%mini)
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) mvar_stat(iHRU)%stat(iVar,iStat) = huge(mvar_stat(iHRU)%stat(iVar,iStat)) 
      if (mvar_data%var(iVar)%dat(1).le.mvar_stat(iHRU)%stat(iVar,iStat)) mvar_stat(iHRU)%stat(iVar,iStat) = mvar_data%var(iVar)%dat(1)

     case (iLookStat%maxi)
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) mvar_stat(iHRU)%stat(iVar,iStat) = -huge(mvar_stat(iHRU)%stat(iVar,iStat)) 
      if (mvar_data%var(iVar)%dat(1).ge.mvar_stat(iHRU)%stat(iVar,iStat)) mvar_stat(iHRU)%stat(iVar,iStat) = mvar_data%var(iVar)%dat(1)

     case (iLookStat%mode)
      mvar_stat(iHRU)%stat(iVar,iStat) = -9999. 

     case (iLookStat%geom)
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) mvar_stat(iHRU)%stat(iVar,iStat) = 0 
      mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat) + 1/mvar_data%var(iVar)%dat(1) 
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.0) mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat)**(1/mvar_meta(iVar)%o_freq(iStat))

     case (iLookStat%harm)
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.1) mvar_stat(iHRU)%stat(iVar,iStat) = 0 
      mvar_stat(iHRU)%stat(iVar,iStat) = mvar_stat(iHRU)%stat(iVar,iStat) * mvar_data%var(iVar)%dat(1) 
      if (mod(mvar_meta(iVar)%o_freq(iStat),iStep).eq.0) mvar_stat(iHRU)%stat(iVar,iStat) = 1/mvar_stat(iHRU)%stat(iVar,iStat) * mvar_meta(iVar)%o_freq(iStat) 

    endselect
   enddo ! iStat 
  enddo ! iVar

 ! --------------------------------------------
 ! (2) loop through forcing variables
 ! --------------------------------------------
  do iVar=1,maxvarForc                            ! loop through variables in structure
   do iStat=1,maxvarStats                         ! loop through output statistics

    if (forc_meta(iVar)%o_freq(iStat).le.0) cycle ! do not bother if output flag is off
    if (forc_meta(iVar)%vartype.ne.iLookVarType%scalarv) cycle ! only calculate stats for scalar vaiables 

    select case(iStat)
     case (iLookStat%inst) ! instantaneous
      forc_stat(iHRU)%stat(iVar,iStat) = forc_data%var(iVar)                                        

     case (iLookStat%mean) ! mean
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) forc_stat(iHRU)%stat(iVar,iStat) = 0. 
      forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat) + forc_data%var(iVar) 
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.0) forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat)/forc_meta(iVar)%o_freq(iStat) 

     case (iLookStat%vari) ! variance
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) then
       forc_stat(iHRU)%stat(iVar,iStat) = 0 
       forc_stat(iHRU)%stat(iVar,maxvarStats+1) = 0 
      endif
      forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat) + forc_data%var(iVar)**2 
      forc_stat(iHRU)%stat(iVar,maxvarStats+1) = forc_stat(iHRU)%stat(iVar,maxvarStats+1) + forc_data%var(iVar)
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.0) then
       forc_stat(iHRU)%stat(iVar,maxvarStats+1) = forc_stat(iHRU)%stat(iVar,maxvarStats+1)/forc_meta(iVar)%o_freq(iStat)
       forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat)/forc_meta(iVar)%o_freq(iStat) + forc_stat(iHRU)%stat(iVar,maxvarStats+1) 
      endif
 
     case (iLookStat%mini)
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) forc_stat(iHRU)%stat(iVar,iStat) = huge(forc_stat(iHRU)%stat(iVar,iStat)) 
      if (forc_data%var(iVar).le.forc_stat(iHRU)%stat(iVar,iStat)) forc_stat(iHRU)%stat(iVar,iStat) = forc_data%var(iVar)
 
     case (iLookStat%maxi)
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) forc_stat(iHRU)%stat(iVar,iStat) = -huge(forc_stat(iHRU)%stat(iVar,iStat)) 
      if (forc_data%var(iVar).ge.forc_stat(iHRU)%stat(iVar,iStat)) forc_stat(iHRU)%stat(iVar,iStat) = forc_data%var(iVar)
 
     case (iLookStat%mode)
      forc_stat(iHRU)%stat(iVar,iStat) = -9999. 

     case (iLookStat%geom)
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) forc_stat(iHRU)%stat(iVar,iStat) = 0 
      forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat) + 1/forc_data%var(iVar) 
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.0) forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat)**(1/forc_meta(iVar)%o_freq(iStat))

     case (iLookStat%harm)
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.1) forc_stat(iHRU)%stat(iVar,iStat) = 0 
      forc_stat(iHRU)%stat(iVar,iStat) = forc_stat(iHRU)%stat(iVar,iStat) * forc_data%var(iVar) 
      if (mod(forc_meta(iVar)%o_freq(iStat),iStep).eq.0) forc_stat(iHRU)%stat(iVar,iStat) = 1/forc_stat(iHRU)%stat(iVar,iStat) * forc_meta(iVar)%o_freq(iStat) 

    endselect
   enddo ! iStat 
  enddo ! iVar

 ! --------------------------------------------
 ! (X) cleanup 
 ! --------------------------------------------

 return
 end subroutine compile_stats

end module module_output
