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

module def_output_module
USE nrtype
USE netcdf
implicit none
private
public :: def_output
public :: netcdf_close
! define dimension names
character(len=32),parameter :: hru_DimName            = 'hru'              ! dimension name for the HRUs
character(len=32),parameter :: scalar_DimName         = 'scalar'           ! dimension name for scalar variables
character(len=32),parameter :: wLength_dimName        = 'spectral_bands'   ! dimension name for the number of spectral bands
character(len=32),parameter :: timestep_DimName       = 'time'             ! dimension name for the time step
character(len=32),parameter :: routing_DimName        = 'timeDelayRouting' ! dimension name for thetime delay routing vectors
character(len=32),parameter :: midSnowAndTime_DimName = 'midSnowAndTime'   ! dimension name for midSnow-time
character(len=32),parameter :: midSoilAndTime_DimName = 'midSoilAndTime'   ! dimension name for midSoil-time
character(len=32),parameter :: midTotoAndTime_DimName = 'midTotoAndTime'   ! dimension name for midToto-time
character(len=32),parameter :: ifcSnowAndTime_DimName = 'ifcSnowAndTime'   ! dimension name for ifcSnow-time
character(len=32),parameter :: ifcSoilAndTime_DimName = 'ifcSoilAndTime'   ! dimension name for ifcSoil-time
character(len=32),parameter :: ifcTotoAndTime_DimName = 'ifcTotoAndTime'   ! dimension name for ifcToto-time
character(len=32),parameter :: Layers_DimName         = 'IntgLayers'       ! dimension name for integrated layers
contains

 ! **********************************************************************************************************
 ! public subroutine def_output: define model output file
 ! **********************************************************************************************************
 subroutine def_output(nHRU,infile,err,message)
 USE multiconst,only:integerMissing
 USE data_struc,only:nFreq,outFreq                 ! output frequencies
 USE data_struc,only:forc_meta,attr_meta,type_meta ! metadata structures
 USE data_struc,only:mpar_meta,mvar_meta,indx_meta ! metadata structures
 USE data_struc,only:bpar_meta,bvar_meta,intg_meta ! metadata structures
 USE data_struc,only:indx_data                     ! for number of soil layers
 USE data_struc,only:model_decisions
 USE data_struc,only:ix_soil                       ! named variable to identify a soil layer
 USE data_struc,only:maxVarStat                    ! # of options for output statistics 
 USE var_lookup,only:maxVarMvar,maxVarMpar,maxVarBvar,maxVarBpar,maxVarForc   
 USE var_lookup,only:maxVarType,maxVarIndx,maxVarAttr,maxVarIntg   
 USE var_lookup,only:iLookVarType                  ! look up for type case select 
 USE var_lookup,only:iLookSTAT                     ! look up for output variable statistics 
 USE var_lookup,only:iLookIndex                    ! index into index structure 

 ! declare dummy variables
 integer(i4b), intent(in)        :: nHRU           ! number of HRUs
 character(*), intent(in)        :: infile         ! file suffix
 integer(i4b),intent(out)        :: err            ! error code
 character(*),intent(out)        :: message        ! error message

 ! local variables
 integer(i4b)                    :: iVar           ! loop through model variables
 integer(i4b)                    :: iStat          ! loop through statistics
 integer(i4b)                    :: iLyr           ! loop through integrated layers
 integer(i4b)                    :: iFreq          ! loop through output frequencies
 integer(i4b)                    :: ncVarID        ! netcdf variable ID
 integer(i4b),dimension(nFreq)   :: ncid           ! array for stroing netcdf file ids
 character(len=5)                :: fstring        ! string to hold model output freuqnecy
 character(len=1000)             :: fname          ! temporary filename
 integer(i4b),parameter          :: modelTime=1    ! model timestep output frequency
 integer(i4b),parameter          :: noStat=0       ! flag for no statistic to be appended to variable name
 integer(i4b),parameter          :: noLyr=0        ! flag for no layer to be appended to variable name
 character(len=256)              :: cmessage       ! temporary error message
 integer(i4b)                    :: nLayers        ! number of integrated layers

 ! initialize errors
 err=0; message="def_output/"

 ! number of layers
 nLayers = count(indx_data%var(iLookINDEX%layerType)%dat==ix_soil)+1

 ! **********************************************************************************************************
 ! ***** create initial files
 ! **********************************************************************************************************
 do iFreq = 1,nFreq
  write(fstring,'(i5)') outFreq(iFreq)
  fstring = adjustl(fstring)
  fname = trim(infile)//'_'//trim(fstring)//'.nc'
  call ini_create(nHRU,trim(fname),ncid(iFreq),err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  print*,'Created output file:',trim(fname)
 enddo

 ! **********************************************************************************************************
 ! ***** define variables/attributes that are only output once
 ! **********************************************************************************************************
 ! define model decisions
 do iVar = 1,size(model_decisions)
  if(model_decisions(iVar)%iDecision.ne.integerMissing)then
   call put_attrib(ncid(modelTime),model_decisions(iVar)%cOption,model_decisions(iVar)%cDecision,err,cmessage)
   if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  endif
 enddo

 ! define local attributes
 do iVar = 1,maxVarAttr
  if (.not.attr_meta(iVar)%statFlg(modelTime,iLookSTAT%inst)) cycle
  call def_variab(ncid(modelTime),(/hru_DimName/),attr_meta(iVar),noStat,noLyr,ncVarID,nf90_double,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  attr_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  attr_meta(iVar)%ncVarID(modelTime,iLookStat%inst) = ncVarID
 enddo  

 ! define local classification of veg, soil, etc.
 do iVar = 1,maxVarType
  if (.not.type_meta(iVar)%statFlg(modelTime,iLookSTAT%inst)) cycle
  call def_variab(ncid(1),(/hru_DimName/),type_meta(iVar),noStat,noLyr,ncVarID,nf90_int,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  type_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  type_meta(iVar)%ncVarID(modelTime,iLookStat%inst) = ncVarID
 enddo  

 ! define local column model parameters
 do iVar = 1,maxVarMpar
  if (.not.mpar_meta(iVar)%statFlg(modelTime,iLookSTAT%inst)) cycle
  call def_variab(ncid(1),(/hru_DimName/),mpar_meta(iVar),noStat,noLyr,ncVarID,nf90_double,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  mpar_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  mpar_meta(iVar)%ncVarID(modelTime,iLookStat%inst) = ncVarID
 enddo  

 ! define basin-average model parameters
 do iVar = 1,maxVarBpar
  if (.not.bpar_meta(iVar)%statFlg(modelTime,iLookSTAT%inst)) cycle
  call def_variab(ncid(1),(/scalar_DimName/),bpar_meta(iVar),noStat,noLyr,ncVarID,nf90_double,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  bpar_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  bpar_meta(iVar)%ncVarID(modelTime,iLookStat%inst) = ncVarID
 enddo  

 ! **********************************************************************************************************
 ! ***** define statistics variables
 ! **********************************************************************************************************
 ! define model forcing data
 do iVar = 1,maxVarForc
  ! put requested timing info into every file
  if(forc_meta(iVar)%varName.eq.'time') then  
   do iFreq = 1,nFreq
    call def_variab(ncid(iFreq),(/Timestep_DimName/),forc_meta(iVar),noStat,noLyr,ncVarID,nf90_double,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    forc_meta(iVar)%ncFilID(iFreq)                = ncid(iFreq)
    forc_meta(iVar)%ncVarID(iFreq,iLookStat%inst) = ncVarID
   enddo
   cycle
  endif
  ! put all other forcing info into appropriate files 
  do iFreq = 1,nFreq
   do iStat = 1,maxVarStat
    if(.not.forc_meta(iVar)%statFlg(iFreq,iStat)) cycle
    call def_variab(ncid(iFreq),(/hru_DimName,Timestep_DimName/),forc_meta(iVar),iStat,noLyr,ncVarID,nf90_double,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    forc_meta(iVar)%ncFilID(iFreq)       = ncid(iFreq)
    forc_meta(iVar)%ncVarID(iFreq,iStat) = ncVarID
   enddo ! looping through output statistics
  enddo ! looping through output frequencies
 enddo ! looping through forcing variables

 ! define vertically integrted variables 
 do iVar = 1,maxVarIntg
  do iFreq = 1,nFreq
   do iStat = 1,maxVarStat 
    if (.not.intg_meta(iVar)%statFlg(iFreq,iStat)) cycle
    call def_variab(ncid(iFreq),(/hru_DimName,Layers_DimName,Timestep_DimName/),intg_meta(iVar),iStat,noLyr,ncVarID,nf90_double,err,cmessage)
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    intg_meta(iVar)%ncFilID(iFreq)       = ncid(iFreq)
    intg_meta(iVar)%ncVarID(iFreq,iStat) = ncVarID
   enddo ! looping through output frequencies
  enddo ! loop through integration layers 
 enddo ! loop through model variables

 ! define local column model variables -- dimensions depend on the variable type
 do iVar = 1,maxVarBvar 
  do iFreq = 1,nFreq
   do iStat = 1,maxVarStat
    if (.not.bvar_meta(iVar)%statFlg(iFreq,iStat)) cycle
    select case(bvar_meta(iVar)%vartype)
     case(iLookVarType%scalarv)
      call def_variab(ncid(iFreq),(/Timestep_DimName/),bvar_meta(iVar),iStat,noLyr,ncVarID,nf90_double,err,cmessage)  
     case(iLookVarType%routing)
      call def_variab(ncid(iFreq),(/Routing_DimName/),bvar_meta(iVar),iStat,noLyr,ncVarID,nf90_double,err,cmessage)  
     case default; err=35; message=trim(message)//"varTypeNotFound"; return
    endselect
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    bvar_meta(iVar)%ncFilID(iFreq)       = ncid(iFreq)
    bvar_meta(iVar)%ncVarID(iFreq,iStat) = ncVarID
   enddo ! loop through statistics
  enddo ! loop through frequencies
 enddo ! loop through model variables

 ! define local column model variables -- only scalars can have non-instantaneous statistics 
 do iVar = 1,maxVarMvar
  do iFreq = 1,nFreq
   do iStat = 1,maxVarStat
    if (.not.mvar_meta(iVar)%statFlg(iFreq,iStat)) cycle
    if (iLookVarType%scalarv.eq.mvar_meta(iVar)%vartype) then
     call def_variab(ncid(iFreq),(/hru_DimName,Timestep_DimName/),mvar_meta(iVar),iStat,noLyr,ncVarID,nf90_double,err,cmessage)
    endif
    if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
    mvar_meta(iVar)%ncFilID(iFreq)       = ncid(iFreq)
    mvar_meta(iVar)%ncVarID(iFreq,iStat) = ncVarID
   enddo ! looping through output statistics
  enddo ! loop through output frequencies
 enddo ! loop through model variables

 ! **********************************************************************************************************
 ! ***** define instantaneous variables
 ! **********************************************************************************************************
 ! define local column model variables -- dimensions depend on the variable type
 do iVar = 1,maxVarMvar
  if (.not.mvar_meta(iVar)%statFlg(modelTime,iLookStat%inst)) cycle
  select case(mvar_meta(iVar)%varType)
   case(iLookVarType%wLength)
    call def_variab(ncid(modelTime),(/hru_DimName,wLength_DimName,Timestep_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage) 
   case(iLookVarType%midSnow)
    call def_variab(ncid(modelTime),(/hru_DimName,midSnowAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage)
   case(iLookVarType%midSoil)
    call def_variab(ncid(modelTime),(/hru_DimName,midSoilAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage) 
   case(iLookVarType%midToto)
    call def_variab(ncid(modelTime),(/hru_DimName,midTotoAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage)  
   case(iLookVarType%ifcSnow)
    call def_variab(ncid(modelTime),(/hru_DimName,ifcSnowAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage)
   case(iLookVarType%ifcSoil)
    call def_variab(ncid(modelTime),(/hru_DimName,ifcSoilAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage) 
   case(iLookVarType%ifcToto)
    call def_variab(ncid(modelTime),(/hru_DimName,ifcTotoAndTime_DimName/),mvar_meta(iVar),iLookStat%inst,noLyr,ncVarID,nf90_double,err,cmessage)  
  endselect
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  mvar_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  mvar_meta(iVar)%ncVarID(modelTime,iLookStat%inst) = ncVarID
 enddo ! loop through model variables
 
 !  define local column model indices
 do iVar = 1,maxVarIndx
  if (.not.indx_meta(iVar)%statFlg(modelTime,iLookSTAT%inst)) cycle
  select case(indx_meta(iVar)%varType)
   case(iLookVarType%scalarv)
    call def_variab(ncid(modelTime),(/hru_DimName,Timestep_DimName/),indx_meta(iVar),noStat,noLyr,ncVarID,nf90_int,err,cmessage) 
   case(iLookVarType%midToto)
    call def_variab(ncid(modelTime),(/hru_DimName,midTotoAndTime_DimName/),indx_meta(iVar),noStat,noLyr,ncVarID,nf90_int,err,cmessage) 
   case default; err=35; message=trim(message)//"varTypeNotFound"; return
  endselect
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  indx_meta(iVar)%ncFilID(modelTime)                = ncid(modelTime)
  indx_meta(iVar)%ncVarID(modelTime,iLookSTAT%inst) = ncVarID
 enddo ! loop through model variables

 return
 end subroutine def_output

 ! **********************************************************************************************************
 ! private subroutine ini_create: initial create
 ! **********************************************************************************************************
 subroutine ini_create(nHRU,infile,ncid,err,message)
 ! variables to define number of steps per file (total number of time steps, step length, etc.)
 USE multiconst,only:secprday           ! number of seconds per day
 USE data_struc,only:data_step          ! time step of model forcing data (s)
 USE data_struc,only:numtim             ! number of time steps
 ! model model index structures
 USE data_struc,only:indx_data          ! data structures
 USE data_struc,only:ix_soil            ! named variable to identify a soil layer
 USE var_lookup,only:iLookINDEX         ! named variables for structure elements
 ! model decisions
 USE data_struc,only:model_decisions    ! model decision structure
 USE var_lookup,only:iLookDECISIONS     ! named variables for elements of the decision structure
 USE mDecisions_module,only:&
  sameRulesAllLayers, & ! SNTHERM option: same combination/sub-dividion rules applied to all layers
  rulesDependLayerIndex ! CLM option: combination/sub-dividion rules depend on layer index
 implicit none

 ! declare dummy variables
 integer(i4b),intent(in)     :: nHRU                       ! number of HRUs
 character(*),intent(in)     :: infile                     ! filename
 integer(i4b),intent(out)    :: err                        ! error code
 character(*),intent(out)    :: message                    ! error message
 integer(i4b),intent(out)    :: ncid                       ! netcdf file id

 ! define local variables
 integer(i4b)                :: dimID
 integer(i4b)                :: maxRouting=1000            ! maximum length of routing vector
 integer(i4b),parameter      :: maxSpectral=2              ! maximum number of spectral bands
 integer(i4b),parameter      :: scalarLength=1             ! length of scalar variable
 integer(i4b)                :: meanSnowLayersPerStep      ! mean number of snow layers per time step
 integer(i4b)                :: maxStepsPerFile            ! maximum number of time steps to be stored in each file
 integer(i4b)                :: maxLength                  ! maximum length of the variable vector
 integer(i4b)                :: nSoil                      ! number of soil layers

 ! initialize error control
 err=0;message="f-iniCreate/"

 ! define number of soil layers
 nSoil = count(indx_data%var(iLookINDEX%layerType)%dat==ix_soil)  ! number of soil layers

 ! identify length of the variable vector
 maxStepsPerFile = min(numtim, nint(366._dp * secprday/data_step) )
 select case(model_decisions(iLookDECISIONS%snowLayers)%iDecision)
  case(sameRulesAllLayers);    meanSnowLayersPerStep = 100
  case(rulesDependLayerIndex); meanSnowLayersPerStep = 5
  case default; err=20; message=trim(message)//'unable to identify option to combine/sub-divide snow layers'; return
 end select ! (option to combine/sub-divide snow layers)
 maxLength = maxStepsPerFile*(nSoil+1 + meanSnowLayersPerStep)
 print*, 'maxStepsPerFile, maxLength = ', maxStepsPerFile, maxLength

 ! create output file
 err = nf90_create(trim(infile),nf90_classic_model,ncid)
 message='iCreate[create]'; call netcdf_err(err,message); if (err/=0) return

 ! create time dimension (unlimited)
 err = nf90_def_dim(ncid, trim(timestep_DimName), nf90_unlimited, dimId)
 message='iCreate[time]'; call netcdf_err(err,message); if (err/=0) return

 ! create scalar dimension
 err = nf90_def_dim(ncid, trim(scalar_DimName), scalarLength, dimId)
 message='iCreate[scalar]'; call netcdf_err(err,message); if (err/=0) return

 ! create HRU dimension
 err = nf90_def_dim(ncid, trim(hru_DimName), nHRU, dimId)
 message='iCreate[HRU]'; call netcdf_err(err,message); if (err/=0) return

 ! create spectral band dimension
 err = nf90_def_dim(ncid, trim(wLength_DimName), maxSpectral, dimId)
 message='iCreate[spectral]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for the time-delay routing variables
 err = nf90_def_dim(ncid, trim(routing_DimName), maxRouting, dimId)
 message='iCreate[routing]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for midSnow+time
 err = nf90_def_dim(ncid, trim(midSnowAndTime_DimName), maxLength, dimId)
 message='iCreate[midSnow]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for midSoil+time
 err = nf90_def_dim(ncid, trim(midSoilAndTime_DimName), maxLength, dimId)
 message='iCreate[midSoil]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for midToto+time
 err = nf90_def_dim(ncid, trim(midTotoAndTime_DimName), maxLength, dimId)
 message='iCreate[minToto]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for ifcSnow+time
 err = nf90_def_dim(ncid, trim(ifcSnowAndTime_DimName), maxLength, dimId)
 message='iCreate[ifcSnow]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for ifcSoil+time
 err = nf90_def_dim(ncid, trim(ifcSoilAndTime_DimName), maxLength, dimId)
 message='iCreate[ifcSoil]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for ifcToto+time
 err = nf90_def_dim(ncid, trim(ifcTotoAndTime_DimName), maxLength, dimId)
 message='iCreate[ifcToto]'; call netcdf_err(err,message); if (err/=0) return

 ! create dimension for integrated layers
 err = nf90_def_dim(ncid, trim(Layers_DimName), nSoil+1, dimId)
 message='iCreate[layered]'; call netcdf_err(err,message); if (err/=0) return

 ! end definition phase
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return
 end subroutine ini_create


 ! **********************************************************************************************************
 ! private subroutine put_attrib: put global attributes as character string
 ! **********************************************************************************************************
 subroutine put_attrib(ncid,attname,attvalue,err,message)
 implicit none
 ! declare dummy variables
 integer(i4b),intent(in)    :: ncid        ! file id
 character(*),intent(in)    :: attname     ! attribute name
 character(*),intent(in)    :: attvalue    ! attribute vaue
 integer(i4b),intent(out)   :: err         ! error code
 character(*),intent(out)   :: message     ! error message

 ! initialize error control
 err=0;message="f-defAttrib/"//trim(attname)//"/"//trim(attvalue)//"/"

 ! allow re-definition of variables
 err = nf90_redef(ncid); call netcdf_err(err,message); if (err/=0) return

 ! put the attribute
 err = nf90_put_att(ncid,nf90_global,trim(attname),trim(attvalue))
 call netcdf_err(err,message); if (err/=0) return

 ! end definition phase
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return

 return
 end subroutine put_attrib


 ! **********************************************************************************************************
 ! private subroutine def_variab: define variables
 ! **********************************************************************************************************
 subroutine def_variab(ncid,dimNames,meta,iStat,iLyr,ncVarID,ivtype,err,message)
 USE get_ixname_module,only:get_StatName   ! for ncdf file variable name
 USE data_struc,only:var_info              ! class def for meta data
 implicit none

 ! dummies
 integer(i4b),intent(in)       :: ncid        ! file id
 character(*),intent(in)       :: dimNames(:) ! dimension namess
 class(var_info),intent(in)    :: meta        ! meta data structure
 integer(i4b),intent(in)       :: iStat       ! output statistic type
 integer(i4b),intent(in)       :: iLyr        ! output integrated layer
 integer(i4b),intent(out)      :: ncVarId     ! variable ID
 integer(i4b),intent(in)       :: ivtype      ! variable type
 integer(i4b),intent(out)      :: err         ! error code
 character(*),intent(out)      :: message     ! error message
 
 ! locals
 integer(i4b)                  :: id          ! loop through dimensions
 integer(i4b)                  :: dimIDs(size(dimNames))
 character(256)                :: catName     ! variable name for netcdf file (includes statistic and layer)
 character(10)                 :: lyrStr      ! string for layer name

 ! initialize error control
 err=0;message="defVariab/"//trim(meta%varname)//"/"

 ! allow re-definition of variables
 err = nf90_redef(ncid); call netcdf_err(err,message); if (err/=0) return

 ! define dimension IDs
 do id = 1,size(dimNames)
  err=nf90_inq_dimid(ncid,trim(dimNames(id)),dimIDs(id))
  call netcdf_err(err,message); if (err/=0) return
 enddo

 ! define variable
 catName = trim(meta%varName)
 if (iStat.gt.0) catName = trim(catName)//'_'//trim(get_statName(iStat))
 if (iLyr.eq.1) then
  lyrStr='snow'
 elseif (iLyr.gt.1) then
  write(lyrStr,'(i10)') iLyr-1
  lyrStr = adjustl(lyrStr)
  lyrStr = 'soil'//trim(lyrStr)
 endif
 if (iLyr.gt.0) catName = trim(catName)//'_'//trim(lyrStr)
 err = nf90_def_var(ncid,trim(catName),ivtype,dimIds,ncVarID)
 call netcdf_err(err,message); if (err/=0) return

 ! add parameter description
 err = nf90_put_att(ncid,ncVarID,'long_name',trim(meta%varDesc))
 call netcdf_err(err,message); if (err/=0) return

 ! add parameter units
 err = nf90_put_att(ncid,ncVarID,'units',trim(meta%varUnit))
 call netcdf_err(err,message); if (err/=0) return

 ! end definition phase
 err = nf90_enddef(ncid); call netcdf_err(err,message); if (err/=0) return

 ! end definition phase
 end subroutine def_variab


 ! **********************************************************************************************************
 ! private subroutine put_attrib: put global attributes as character string
 ! **********************************************************************************************************
 subroutine netcdf_close(err,message)
 USE multiconst,only:integerMissing                   ! missing value flag
 USE data_struc,only:nFreq                            ! output frequencies
 USE data_struc,only:forc_meta,attr_meta,type_meta    ! metadata structures
 USE data_struc,only:mpar_meta,mvar_meta,indx_meta    ! metadata structures
 USE data_struc,only:bpar_meta,bvar_meta,intg_meta    ! metadata structures
 USE var_lookup,only:maxVarForc,maxVarAttr,maxVarType ! variable counts
 USE var_lookup,only:maxVarMpar,maxVarMvar,maxVarIndx ! variable counts
 USE var_lookup,only:maxVarBpar,maxVarBvar,maxVarIntg ! variable counts
 implicit none

 ! declare dummy variables
 integer(i4b),intent(out)   :: err          ! error code
 character(*),intent(out)   :: message      ! error message

 ! internals
 integer(i4b) :: iVar
 integer(i4b) :: iFreq

 ! initialize error control
 err=0; message = 'netcdf_close/'

 ! loop through all variables to close all files
 do iVar = 1,maxVarForc ! forcings
  do iFreq = 1,nFreq
   if (forc_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(forc_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    forc_meta(iVar)%ncFilID = integerMissing
    forc_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarAttr ! attributes
  do iFreq = 1,nFreq
   if (attr_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(attr_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    attr_meta(iVar)%ncFilID = integerMissing
    attr_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarType ! types
  do iFreq = 1,nFreq
   if (type_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(type_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    type_meta(iVar)%ncFilID = integerMissing
    type_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarMpar ! model parameters
  do iFreq = 1,nFreq
   if (mpar_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(mpar_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    mpar_meta(iVar)%ncFilID = integerMissing
    mpar_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarMvar ! model variables
  do iFreq = 1,nFreq
   if (mvar_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(mvar_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    mvar_meta(iVar)%ncFilID = integerMissing
    mvar_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarBpar ! basin parameters
  do iFreq = 1,nFreq
   if (bpar_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(bpar_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    bpar_meta(iVar)%ncFilID = integerMissing
    bpar_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarBvar ! basin variables
  do iFreq = 1,nFreq
   if (bvar_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(bvar_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    bvar_meta(iVar)%ncFilID = integerMissing
    bvar_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarIntg ! integrated variables
  do iFreq = 1,nFreq
   if (intg_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(intg_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    intg_meta(iVar)%ncFilID = integerMissing
    intg_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 do iVar = 1,maxVarIndx ! index variables
  do iFreq = 1,nFreq
   if (indx_meta(iVar)%ncFilID(iFreq).ne.integerMissing) then
    err = nf90_close(indx_meta(iVar)%ncFilID(iFreq))
    if ((err.ne.0).and.(err.ne.-33)) then; call netcdf_err(err,message); message='netcdf_close'//trim(message); return; endif
    indx_meta(iVar)%ncFilID = integerMissing
    indx_meta(iVar)%ncVarID = integerMissing
   endif ! file exists
  enddo ! iFreq
 enddo ! iVar

 if (err.eq.-33) err=0
 return
 end subroutine netcdf_close

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
 endif
 end subroutine netcdf_err

end module def_output_module
