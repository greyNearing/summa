module popMetadat_module
implicit none
private
public::popMetadat
contains
 
 subroutine popMetadat(err,message)
 USE nrtype
 USE multiconst,only:integerMissing
 ! data structures
 USE data_struc, only: var_info   ! data type for metadata structure
 USE data_struc, only: time_meta  ! data structure for time metadata
 USE data_struc, only: forc_meta  ! data structure for forcing metadata
 USE data_struc, only: type_meta  ! data structure for categorical metadata
 USE data_struc, only: attr_meta  ! data structure for attribute metadata
 USE data_struc, only: mpar_meta  ! data structure for local parameter metadata
 USE data_struc, only: bpar_meta  ! data structure for basin parameter metadata
 USE data_struc, only: mvar_meta  ! data structure for local model variable metadata
 USE data_struc, only: bvar_meta  ! data structure for basin model variable metadata
 USE data_struc, only: indx_meta  ! data structure for index metadata
 ! structures of named variables
 USE var_lookup, only: iLookTIME  ! named variables for time data structure
 USE var_lookup, only: iLookFORCE ! named variables for forcing data structure
 USE var_lookup, only: iLookTYPE  ! named variables for categorical attribute data structure
 USE var_lookup, only: iLookATTR  ! named variables for real valued attribute data structure
 USE var_lookup, only: iLookPARAM ! named variables for local parameter data structure
 USE var_lookup, only: iLookBPAR  ! named variables for basin parameter data structure
 USE var_lookup, only: iLookMVAR  ! named variables for local model variable data structure
 USE var_lookup, only: iLookBVAR  ! named variables for basin model variable data structure
 USE var_lookup, only: iLookINDEX ! named variables for index variable data structure
 ! index into various data types - for readability and backward-compatibility
 USE get_ixname_module, only: get_ixVarType  ! varType index function
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err                            ! error code
 character(*),intent(out)       :: message                        ! error message
 ! local variables
 character(LEN=256)             :: cmessage                       ! error message of downwind routine
 ! initialize error control
 err=0; message='popMetadat/'

 ! -----
 ! * model time structures...
 ! --------------------------
 time_meta(iLookTIME%iyyy) = var_info('iyyy', 'year'  , '-', get_ixVarType('scalarv'), integerMissing, 0)
 time_meta(iLookTIME%im)   = var_info('im'  , 'month' , '-', get_ixVarType('scalarv'), integerMissing, 0)
 time_meta(iLookTIME%id)   = var_info('id'  , 'day'   , '-', get_ixVarType('scalarv'), integerMissing, 0)
 time_meta(iLookTIME%ih)   = var_info('ih'  , 'hour'  , '-', get_ixVarType('scalarv'), integerMissing, 0)
 time_meta(iLookTIME%imin) = var_info('imin', 'minute', '-', get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * model forcing data...
 ! -----------------------
 forc_meta(iLookFORCE%time)     = var_info('time'    , 'time since time reference'                         , 'seconds since 1990-1-1 0:0:0.0 -0:00', get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%pptrate)  = var_info('pptrate' , 'precipitation rate'                                , 'kg m-2 s-1'                          , get_ixVarType('scalarv'), integerMissing, 0) 
 forc_meta(iLookFORCE%SWRadAtm) = var_info('SWRadAtm', 'downward shortwave radiation at the upper boundary', 'W m-2'                               , get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%LWRadAtm) = var_info('LWRadAtm', 'downward longwave radiation at the upper boundary' , 'W m-2'                               , get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%airtemp)  = var_info('airtemp' , 'air temperature at the measurement height'         , 'K'                                   , get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%windspd)  = var_info('windspd' , 'wind speed at the measurement height'              , 'm s-1'                               , get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%airpres)  = var_info('airpres' , 'air pressure at the the measurement height'        , 'Pa'                                  , get_ixVarType('scalarv'), integerMissing, 0)
 forc_meta(iLookFORCE%spechum)  = var_info('spechum' , 'specific humidity at the measurement height'       , 'g g-1'                               , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * categorical data...
 ! ---------------------
 type_meta(iLookTYPE%hruIndex)       = var_info('hruIndex'      , 'index defining the hydrologic response unit', '-', get_ixVarType('scalarv'), integerMissing, 0)
 type_meta(iLookTYPE%vegTypeIndex)   = var_info('vegTypeIndex'  , 'index defining vegetation type'             , '-', get_ixVarType('scalarv'), integerMissing, 0)
 type_meta(iLookTYPE%soilTypeIndex)  = var_info('soilTypeIndex' , 'index defining soil type'                   , '-', get_ixVarType('scalarv'), integerMissing, 0)
 type_meta(iLookTYPE%slopeTypeIndex) = var_info('slopeTypeIndex', 'index defining slope'                       , '-', get_ixVarType('scalarv'), integerMissing, 0)
 type_meta(iLookTYPE%downHRUindex)   = var_info('downHRUindex'  , 'index of downslope HRU (0 = basin outlet)'  , '-', get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * site characteristics...
 ! -------------------------
 attr_meta(iLookATTR%latitude)       = var_info('latitude'      , 'latitude'                                              , 'degrees north', get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%longitude)      = var_info('longitude'     , 'longitude'                                             , 'degrees east' , get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%elevation)      = var_info('elevation'     , 'elevation'                                             , 'm'            , get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%tan_slope)      = var_info('tan_slope'     , 'tan water table slope (tan local ground surface slope)', '-'            , get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%contourLength)  = var_info('contourLength' , 'length of contour at downslope edge of HRU'            , 'm'            , get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%HRUarea)        = var_info('HRUarea'       , 'area of each HRU'                                      , 'm2'           , get_ixVarType('scalarv'), integerMissing, 0)
 attr_meta(iLookATTR%mHeight)        = var_info('mHeight'       , 'measurement height above bare ground'                  , 'm'            , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * local parameter data...
 ! ------------------------- 
 ! boundary conditions
 mpar_meta(iLookPARAM%upperBoundHead)        = var_info('upperBoundHead'        , 'matric head at the upper boundary'                                , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%lowerBoundHead)        = var_info('lowerBoundHead'        , 'matric head at the lower boundary'                                , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%upperBoundTheta)       = var_info('upperBoundTheta'       , 'volumetric liquid water content at the upper boundary'            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%lowerBoundTheta)       = var_info('lowerBoundTheta'       , 'volumetric liquid water content at the lower boundary'            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%upperBoundTemp)        = var_info('upperBoundTemp'        , 'temperature of the upper boundary'                                , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%lowerBoundTemp)        = var_info('lowerBoundTemp'        , 'temperature of the lower boundary'                                , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! precipitation partitioning
 mpar_meta(iLookPARAM%tempCritRain)          = var_info('tempCritRain'          , 'critical temperature where precipitation is rain'                 , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%tempRangeTimestep)     = var_info('tempRangeTimestep'     , 'temperature range over the time step'                             , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%frozenPrecipMultip)    = var_info('frozenPrecipMultip'    , 'frozen precipitation multiplier'                                  , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! freezing curve for snow
 mpar_meta(iLookPARAM%snowfrz_scale)         = var_info('snowfrz_scale'         , 'scaling parameter for the freezing curve for snow'                , 'K-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 ! snow albedo
 mpar_meta(iLookPARAM%albedoMax)             = var_info('albedoMax'             , 'maximum snow albedo (single spectral band)'                       , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMinWinter)       = var_info('albedoMinWinter'       , 'minimum snow albedo during winter (single spectral band)'         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMinSpring)       = var_info('albedoMinSpring'       , 'minimum snow albedo during spring (single spectral band)'         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMaxVisible)      = var_info('albedoMaxVisible'      , 'maximum snow albedo in the visible part of the spectrum'          , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMinVisible)      = var_info('albedoMinVisible'      , 'minimum snow albedo in the visible part of the spectrum'          , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMaxNearIR)       = var_info('albedoMaxNearIR'       , 'maximum snow albedo in the near infra-red part of the spectrum'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoMinNearIR)       = var_info('albedoMinNearIR'       , 'minimum snow albedo in the near infra-red part of the spectrum'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoDecayRate)       = var_info('albedoDecayRate'       , 'albedo decay rate'                                                , 's'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoSootLoad)        = var_info('albedoSootLoad'        , 'soot load factor'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%albedoRefresh)         = var_info('albedoRefresh'         , 'critical mass necessary for albedo refreshment'                   , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 ! radiation transfer
 mpar_meta(iLookPARAM%radExt_snow)           = var_info('radExt_snow'           , 'extinction coefficient for radiation penetration into snowpack'   , 'm-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%directScale)           = var_info('directScale'           , 'scaling factor for fractional driect radiaion parameterization'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Frad_direct)           = var_info('Frad_direct'           , 'fraction direct solar radiation'                                  , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Frad_vis)              = var_info('Frad_vis'              , 'fraction radiation in visible part of spectrum'                   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! new snow density
 mpar_meta(iLookPARAM%newSnowDenMin)         = var_info('newSnowDenMin'         , 'minimum new snow density'                                         , 'kg m-3'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%newSnowDenMult)        = var_info('newSnowDenMult'        , 'multiplier for new snow density'                                  , 'kg m-3'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%newSnowDenScal)        = var_info('newSnowDenScal'        , 'scaling factor for new snow density'                              , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! snow compaction
 mpar_meta(iLookPARAM%densScalGrowth)        = var_info('densScalGrowth'        , 'density scaling factor for grain growth'                          , 'kg-1 m3'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%tempScalGrowth)        = var_info('tempScalGrowth'        , 'temperature scaling factor for grain growth'                      , 'K-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%grainGrowthRate)       = var_info('grainGrowthRate'       , 'rate of grain growth'                                             , 's-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%densScalOvrbdn)        = var_info('densScalOvrbdn'        , 'density scaling factor for overburden pressure'                   , 'kg-1 m3'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%tempScalOvrbdn)        = var_info('tempScalOvrbdn'        , 'temperature scaling factor for overburden pressure'               , 'K-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%base_visc)             = var_info('base_visc'             , 'viscosity coefficient at T=T_frz and snow density=0'              , 'kg s m-2'        , get_ixVarType('scalarv'), integerMissing, 0)
 ! water flow through snow
 mpar_meta(iLookPARAM%Fcapil)                = var_info('Fcapil'                , 'capillary retention (fraction of total pore volume)'              , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%k_snow)                = var_info('k_snow'                , 'hydraulic conductivity of snow'                                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%mw_exp)                = var_info('mw_exp'                , 'exponent for meltwater flow'                                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! turbulent heat fluxes
 mpar_meta(iLookPARAM%z0Snow)                = var_info('z0Snow'                , 'roughness length of snow'                                         , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%z0Soil)                = var_info('z0Soil'                , 'roughness length of bare soil below the canopy'                   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%z0Canopy)              = var_info('z0Canopy'              , 'roughness length of the canopy'                                   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zpdFraction)           = var_info('zpdFraction'           , 'zero plane displacement / canopy height'                          , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%critRichNumber)        = var_info('critRichNumber'        , 'critical value for the bulk Richardson number'                    , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Louis79_bparam)        = var_info('Louis79_bparam'        , 'parameter in Louis (1979) stability function'                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Louis79_cStar)         = var_info('Louis79_cStar'         , 'parameter in Louis (1979) stability function'                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Mahrt87_eScale)        = var_info('Mahrt87_eScale'        , 'exponential scaling factor in the Mahrt (1987) stability function', '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%leafExchangeCoeff)     = var_info('leafExchangeCoeff'     , 'turbulent exchange coeff between canopy surface and canopy air'   , 'm s-(1/2)'       , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%windReductionParam)    = var_info('windReductionParam'    , 'canopy wind reduction parameter'                                  , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! stomatal conductance
 mpar_meta(iLookPARAM%Kc25)                  = var_info('Kc25'                  , 'Michaelis-Menten constant for CO2 at 25 degrees C'                , 'umol mol-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Ko25)                  = var_info('Ko25'                  , 'Michaelis-Menten constant for O2 at 25 degrees C'                 , 'mol mol-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Kc_qFac)               = var_info('Kc_qFac'               , 'factor in the q10 function defining temperature controls on Kc'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%Ko_qFac)               = var_info('Ko_qFac'               , 'factor in the q10 function defining temperature controls on Ko'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%kc_Ha)                 = var_info('kc_Ha'                 , 'activation energy for the Michaelis-Menten constant for CO2'      , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%ko_Ha)                 = var_info('ko_Ha'                 , 'activation energy for the Michaelis-Menten constant for O2'       , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax25_canopyTop)     = var_info('vcmax25_canopyTop'     , 'potential carboxylation rate at 25 degrees C at the canopy top'   , 'umol co2 m-2 s-1', get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax_qFac)            = var_info('vcmax_qFac'            , 'factor in the q10 function defining temperature controls on vcmax', '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax_Ha)              = var_info('vcmax_Ha'              , 'activation energy in the vcmax function'                          , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax_Hd)              = var_info('vcmax_Hd'              , 'deactivation energy in the vcmax function'                        , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax_Sv)              = var_info('vcmax_Sv'              , 'entropy term in the vcmax function'                               , 'J mol-1 K-1'     , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vcmax_Kn)              = var_info('vcmax_Kn'              , 'foliage nitrogen decay coefficient'                               , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%jmax25_scale)          = var_info('jmax25_scale'          , 'scaling factor to relate jmax25 to vcmax25'                       , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%jmax_Ha)               = var_info('jmax_Ha'               , 'activation energy in the jmax function'                           , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%jmax_Hd)               = var_info('jmax_Hd'               , 'deactivation energy in the jmax function'                         , 'J mol-1'         , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%jmax_Sv)               = var_info('jmax_Sv'               , 'entropy term in the jmax function'                                , 'J mol-1 K-1'     , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%fractionJ)             = var_info('fractionJ'             , 'fraction of light lost by other than the chloroplast lamellae'    , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%quantamYield)          = var_info('quantamYield'          , 'quantam yield'                                                    , 'mol e mol-1 q'   , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vpScaleFactor)         = var_info('vpScaleFactor'         , 'vapor pressure scaling factor in stomatal conductance function'   , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%cond2photo_slope)      = var_info('cond2photo_slope'      , 'slope of conductance-photosynthesis relationship'                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%minStomatalConductance)= var_info('minStomatalConductance', 'minimum stomatal conductance'                                     , 'umol H2O m-2 s-1', get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation properties
 mpar_meta(iLookPARAM%winterSAI)             = var_info('winterSAI'             , 'stem area index prior to the start of the growing season'         , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%summerLAI)             = var_info('summerLAI'             , 'maximum leaf area index at the peak of the growing season'        , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%rootScaleFactor1)      = var_info('rootScaleFactor1'      , '1st scaling factor (a) in Y = 1 - 0.5*( exp(-aZ) + exp(-bZ) )'    , 'm-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%rootScaleFactor2)      = var_info('rootScaleFactor2'      , '2nd scaling factor (b) in Y = 1 - 0.5*( exp(-aZ) + exp(-bZ) )'    , 'm-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%rootingDepth)          = var_info('rootingDepth'          , 'rooting depth'                                                    , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%rootDistExp)           = var_info('rootDistExp'           , 'exponent for the vertical distribution of root density'           , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%plantWiltPsi)          = var_info('plantWiltPsi'          , 'matric head at wilting point'                                     , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%soilStressParam)       = var_info('soilStressParam'       , 'parameter in the exponential soil stress function'                , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%critSoilWilting)       = var_info('critSoilWilting'       , 'critical vol. liq. water content when plants are wilting'         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%critSoilTranspire)     = var_info('critSoilTranspire'     , 'critical vol. liq. water content when transpiration is limited'   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%critAquiferTranspire)  = var_info('critAquiferTranspire'  , 'critical aquifer storage value when transpiration is limited'     , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%minStomatalResistance) = var_info('minStomatalResistance' , 'minimum stomatal resistance'                                      , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%leafDimension)         = var_info('leafDimension'         , 'characteristic leaf dimension'                                    , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%heightCanopyTop)       = var_info('heightCanopyTop'       , 'height of top of the vegetation canopy above ground surface'      , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%heightCanopyBottom)    = var_info('heightCanopyBottom'    , 'height of bottom of the vegetation canopy above ground surface'   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%specificHeatVeg)       = var_info('specificHeatVeg'       , 'specific heat of vegetation'                                      , 'J kg-1 K-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%maxMassVegetation)     = var_info('maxMassVegetation'     , 'maximum mass of vegetation (full foliage)'                        , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%throughfallScaleSnow)  = var_info('throughfallScaleSnow'  , 'scaling factor for throughfall (snow)'                            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%throughfallScaleRain)  = var_info('throughfallScaleRain'  , 'scaling factor for throughfall (rain)'                            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%refInterceptCapSnow)   = var_info('refInterceptCapSnow'   , 'reference canopy interception capacity per unit leaf area (snow)' , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%refInterceptCapRain)   = var_info('refInterceptCapRain'   , 'canopy interception capacity per unit leaf area (rain)'           , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%snowUnloadingCoeff)    = var_info('snowUnloadingCoeff'    , 'time constant for unloading of snow from the forest canopy'       , 's-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%canopyDrainageCoeff)   = var_info('canopyDrainageCoeff'   , 'time constant for drainage of liquid water from the forest canopy', 's-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%ratioDrip2Unloading)   = var_info('ratioDrip2Unloading'   , 'ratio of canopy drip to unloading of snow from the forest canopy' , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%canopyWettingFactor)   = var_info('canopyWettingFactor'   , 'maximum wetted fraction of the canopy'                            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%canopyWettingExp)      = var_info('canopyWettingExp'      , 'exponent in canopy wetting function'                              , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! soil properties
 mpar_meta(iLookPARAM%soil_dens_intr)        = var_info('soil_dens_intr'        , 'intrinsic soil density'                                           , 'kg m-3'          , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%thCond_soil)           = var_info('thCond_soil'           , 'thermal conductivity of soil (includes quartz and other minerals)', 'W m-1 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%frac_sand)             = var_info('frac_sand'             , 'fraction of sand'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%frac_silt)             = var_info('frac_silt'             , 'fraction of silt'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%frac_clay)             = var_info('frac_clay'             , 'fraction of clay'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%fieldCapacity)         = var_info('fieldCapacity'         , 'soil field capacity (vol liq water content when baseflow begins)' , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%wettingFrontSuction)   = var_info('wettingFrontSuction'   , 'Green-Ampt wetting front suction'                                 , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%theta_mp)              = var_info('theta_mp'              , 'volumetric liquid water content when macropore flow begins'       , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%theta_sat)             = var_info('theta_sat'             , 'soil porosity'                                                    , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%theta_res)             = var_info('theta_res'             , 'volumetric residual water content'                                , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vGn_alpha)             = var_info('vGn_alpha'             , 'van Genuchten "alpha" parameter'                                  , 'm-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%vGn_n)                 = var_info('vGn_n'                 , 'van Genuchten "n" parameter'                                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%mpExp)                 = var_info('mpExp'                 , 'empirical exponent in macropore flow equation'                    , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%k_soil)                = var_info('k_soil'                , 'saturated hydraulic conductivity'                                 , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%k_macropore)           = var_info('k_macropore'           , 'saturated hydraulic conductivity for macropores'                  , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%kAnisotropic)          = var_info('kAnisotropic'          , 'anisotropy factor for lateral hydraulic conductivity'             , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zScale_TOPMODEL)       = var_info('zScale_TOPMODEL'       , 'TOPMODEL scaling factor used in lower boundary condition for soil', 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%compactedDepth)        = var_info('compactedDepth'        , 'depth where k_soil reaches the compacted value given by CH78'     , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%aquiferScaleFactor)    = var_info('aquiferScaleFactor'    , 'scaling factor for aquifer storage in the big bucket'             , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%aquiferBaseflowExp)    = var_info('aquiferBaseflowExp'    , 'baseflow exponent'                                                , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%qSurfScale)            = var_info('qSurfScale'            , 'scaling factor in the surface runoff parameterization'            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%specificYield)         = var_info('specificYield'         , 'specific yield'                                                   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%specificStorage)       = var_info('specificStorage'       , 'specific storage coefficient'                                     , 'm-1'             , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%f_impede)              = var_info('f_impede'              , 'ice impedence factor'                                             , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%soilIceScale)          = var_info('soilIceScale'          , 'scaling factor for depth of soil ice, used to get frozen fraction', 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%soilIceCV)             = var_info('soilIceCV'             , 'CV of depth of soil ice, used to get frozen fraction'             , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! algorithmic control parameters
 mpar_meta(iLookPARAM%minwind)               = var_info('minwind'               , 'minimum wind speed'                                               , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%minstep)               = var_info('minstep'               , 'minimum length of the time step'                                  , 's'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%maxstep)               = var_info('maxstep'               , 'maximum length of the time step'                                  , 's'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%wimplicit)             = var_info('wimplicit'             , 'weight assigned to the start-of-step fluxes (alpha)'              , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%maxiter)               = var_info('maxiter'               , 'maximum number of iterations'                                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%relConvTol_liquid)     = var_info('relConvTol_liquid'     , 'relative convergence tolerance for vol frac liq water'            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%absConvTol_liquid)     = var_info('absConvTol_liquid'     , 'absolute convergence tolerance for vol frac liq water'            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%relConvTol_matric)     = var_info('relConvTol_matric'     , 'relative convergence tolerance for matric head'                   , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%absConvTol_matric)     = var_info('absConvTol_matric'     , 'absolute convergence tolerance for matric head'                   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%relConvTol_energy)     = var_info('relConvTol_energy'     , 'relative convergence tolerance for energy'                        , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%absConvTol_energy)     = var_info('absConvTol_energy'     , 'absolute convergence tolerance for energy'                        , 'J m-3'           , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%relConvTol_aquifr)     = var_info('relConvTol_aquifr'     , 'relative convergence tolerance for aquifer storage'               , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%absConvTol_aquifr)     = var_info('absConvTol_aquifr'     , 'absolute convergence tolerance for aquifer storage'               , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmin)                  = var_info('zmin'                  , 'minimum layer depth'                                              , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmax)                  = var_info('zmax'                  , 'maximum layer depth'                                              , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zminLayer1)            = var_info('zminLayer1'            , 'minimum layer depth for the 1st (top) layer'                      , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zminLayer2)            = var_info('zminLayer2'            , 'minimum layer depth for the 2nd layer'                            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zminLayer3)            = var_info('zminLayer3'            , 'minimum layer depth for the 3rd layer'                            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zminLayer4)            = var_info('zminLayer4'            , 'minimum layer depth for the 4th layer'                            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zminLayer5)            = var_info('zminLayer5'            , 'minimum layer depth for the 5th (bottom) layer'                   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer1_lower)      = var_info('zmaxLayer1_lower'      , 'maximum layer depth for the 1st (top) layer when only 1 layer'    , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer2_lower)      = var_info('zmaxLayer2_lower'      , 'maximum layer depth for the 2nd layer when only 2 layers'         , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer3_lower)      = var_info('zmaxLayer3_lower'      , 'maximum layer depth for the 3rd layer when only 3 layers'         , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer4_lower)      = var_info('zmaxLayer4_lower'      , 'maximum layer depth for the 4th layer when only 4 layers'         , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer1_upper)      = var_info('zmaxLayer1_upper'      , 'maximum layer depth for the 1st (top) layer when > 1 layer'       , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer2_upper)      = var_info('zmaxLayer2_upper'      , 'maximum layer depth for the 2nd layer when > 2 layers'            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer3_upper)      = var_info('zmaxLayer3_upper'      , 'maximum layer depth for the 3rd layer when > 3 layers'            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mpar_meta(iLookPARAM%zmaxLayer4_upper)      = var_info('zmaxLayer3_upper'      , 'maximum layer depth for the 4th layer when > 4 layers'            , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * basin parameter data...
 ! -------------------------
 bpar_meta(iLookBPAR%basin__aquiferHydCond)     = var_info('basin__aquiferHydCond'    , 'hydraulic conductivity of the aquifer'                          , 'm s-1', get_ixVarType('scalarv'), integerMissing, 0)
 bpar_meta(iLookBPAR%basin__aquiferScaleFactor) = var_info('basin__aquiferScaleFactor', 'scaling factor for aquifer storage in the big bucket'           , 'm'    , get_ixVarType('scalarv'), integerMissing, 0)
 bpar_meta(iLookBPAR%basin__aquiferBaseflowExp) = var_info('basin__aquiferBaseflowExp', 'baseflow exponent for the big bucket'                           , '-'    , get_ixVarType('scalarv'), integerMissing, 0)
 bpar_meta(iLookBPAR%routingGammaShape)         = var_info('routingGammaShape'        , 'shape parameter in Gamma distribution used for sub-grid routing', '-'    , get_ixVarType('scalarv'), integerMissing, 0)
 bpar_meta(iLookBPAR%routingGammaScale)         = var_info('routingGammaScale'        , 'scale parameter in Gamma distribution used for sub-grid routing', 's'    , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * local model variables...
 ! --------------------------
 ! timestep-average fluxes for a few key variables
 mvar_meta(iLookMVAR%totalSoilCompress)               = var_info('totalSoilCompress'              , 'change in total soil storage due to compression of soil matrix'   , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageThroughfallSnow)          = var_info('averageThroughfallSnow'         , 'snow that reaches the ground without ever touching the canopy'    , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageThroughfallRain)          = var_info('averageThroughfallRain'         , 'rain that reaches the ground without ever touching the canopy'    , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopySnowUnloading)      = var_info('averageCanopySnowUnloading'     , 'unloading of snow from the vegetion canopy'                       , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopyLiqDrainage)        = var_info('averageCanopyLiqDrainage'       , 'drainage of liquid water from the vegetation canopy'              , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopyMeltFreeze)         = var_info('averageCanopyMeltFreeze'        , 'melt/freeze of water stored in the canopy'                        , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopyTranspiration)      = var_info('averageCanopyTranspiration'     , 'canopy transpiration'                                             , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopyEvaporation)        = var_info('averageCanopyEvaporation'       , 'canopy evaporation/condensation'                                  , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageCanopySublimation)        = var_info('averageCanopySublimation'       , 'canopy sublimation/frost'                                         , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageSnowSublimation)          = var_info('averageSnowSublimation'         , 'snow sublimation/frost (below canopy or non-vegetated)'           , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageGroundEvaporation)        = var_info('averageGroundEvaporation'       , 'ground evaporation/condensation (below canopy or non-vegetated)'  , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageRainPlusMelt)             = var_info('averageRainPlusMelt'            , 'rain plus melt input to soil before calculating surface runoff'   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageSurfaceRunoff)            = var_info('averageSurfaceRunoff'           , 'surface runoff'                                                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageSoilInflux)               = var_info('averageSoilInflux'              , 'influx of water at the top of the soil profile'                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageSoilBaseflow)             = var_info('averageSoilBaseflow'            , 'total baseflow from throughout the soil profile'                  , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageSoilDrainage)             = var_info('averageSoilDrainage'            , 'drainage from the bottom of the soil profile'                     , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageAquiferRecharge)          = var_info('averageAquiferRecharge'         , 'recharge to the aquifer'                                          , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageAquiferBaseflow)          = var_info('averageAquiferBaseflow'         , 'baseflow from the aquifer'                                        , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageAquiferTranspire)         = var_info('averageAquiferTranspire'        , 'transpiration from the aquifer'                                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%averageColumnOutflow)            = var_info('averageColumnOutflow'           , 'outflow from each layer in the soil profile'                      , 'm3 s-1'          , get_ixVarType('midSoil'), integerMissing, 0)
 ! scalar variables (forcing)
 mvar_meta(iLookMVAR%scalarCosZenith)                 = var_info('scalarCosZenith'                , 'cosine of the solar zenith angle'                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarFractionDirect)            = var_info('scalarFractionDirect'           , 'fraction of direct radiation (0-1)'                               , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralIncomingDirect)          = var_info('spectralIncomingDirect'         , 'incoming direct solar radiation in each wave band'                , 'W m-2'           , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralIncomingDiffuse)         = var_info('spectralIncomingDiffuse'        , 'incoming diffuse solar radiation in each wave band'               , 'W m-2'           , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVPair)                     = var_info('scalarVPair'                    , 'vapor pressure of the air above the vegetation canopy'            , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarTwetbulb)                  = var_info('scalarTwetbulb'                 , 'wet bulb temperature'                                             , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarRainfall)                  = var_info('scalarRainfall'                 , 'computed rainfall rate'                                           , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowfall)                  = var_info('scalarSnowfall'                 , 'computed snowfall rate'                                           , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowfallTemp)              = var_info('scalarSnowfallTemp'             , 'temperature of fresh snow'                                        , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarNewSnowDensity)            = var_info('scalarNewSnowDensity'           , 'density of fresh snow (should snow be falling in this time step)' , 'kg m-3'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarO2air)                     = var_info('scalarO2air'                    , 'atmospheric o2 concentration'                                     , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCO2air)                    = var_info('scalarCO2air'                   , 'atmospheric co2 concentration'                                    , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 ! scalar variables (state variables)
 mvar_meta(iLookMVAR%scalarCanopyIce)                 = var_info('scalarCanopyIce'                , 'mass of ice on the vegetation canopy'                             , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyLiq)                 = var_info('scalarCanopyLiq'                , 'mass of liquid water on the vegetation canopy'                    , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanairTemp)                = var_info('scalarCanairTemp'               , 'temperature of the canopy air space'                              , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyTemp)                = var_info('scalarCanopyTemp'               , 'temperature of the vegetation canopy'                             , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowAge)                   = var_info('scalarSnowAge'                  , 'non-dimensional snow age'                                         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowAlbedo)                = var_info('scalarSnowAlbedo'               , 'snow albedo for the entire spectral band'                         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralSnowAlbedoDirect)        = var_info('spectralSnowAlbedoDirect'       , 'direct snow albedo for individual spectral bands'                 , '-'               , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralSnowAlbedoDiffuse)       = var_info('spectralSnowAlbedoDiffuse'      , 'diffuse snow albedo for individual spectral bands'                , '-'               , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowDepth)                 = var_info('scalarSnowDepth'                , 'total snow depth'                                                 , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSWE)                       = var_info('scalarSWE'                      , 'snow water equivalent'                                            , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSfcMeltPond)               = var_info('scalarSfcMeltPond'              , 'ponded water caused by melt of the "snow without a layer"'        , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferStorage)            = var_info('scalarAquiferStorage'           , 'water required to bring aquifer to the bottom of the soil profile', 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSurfaceTemp)               = var_info('scalarSurfaceTemp'              , 'surface temperature (just a copy of the upper-layer temperature)' , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (general)
 mvar_meta(iLookMVAR%scalarGreenVegFraction)          = var_info('scalarGreenVegFraction'         , 'green vegetation fraction (used to compute LAI)'                  , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarBulkVolHeatCapVeg)         = var_info('scalarBulkVolHeatCapVeg'        , 'bulk volumetric heat capacity of vegetation'                      , 'J m-3 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarRootZoneTemp)              = var_info('scalarRootZoneTemp'             , 'average temperature of the root zone'                             , 'K'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLAI)                       = var_info('scalarLAI'                      , 'one-sided leaf area index'                                        , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSAI)                       = var_info('scalarSAI'                      , 'one-sided stem area index'                                        , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarExposedLAI)                = var_info('scalarExposedLAI'               , 'exposed leaf area index (after burial by snow)'                   , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarExposedSAI)                = var_info('scalarExposedSAI'               , 'exposed stem area index (after burial by snow)'                   , 'm2 m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyIceMax)              = var_info('scalarCanopyIceMax'             , 'maximum interception storage capacity for ice'                    , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyLiqMax)              = var_info('scalarCanopyLiqMax'             , 'maximum interception storage capacity for liquid water'           , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGrowingSeasonIndex)        = var_info('scalarGrowingSeasonIndex'       , 'growing season index (0=off, 1=on)'                               , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVP_CanopyAir)              = var_info('scalarVP_CanopyAir'             , 'vapor pressure of the canopy air space'                           , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (shortwave radiation)
 mvar_meta(iLookMVAR%scalarCanopySunlitFraction)      = var_info('scalarCanopySunlitFraction'     , 'sunlit fraction of canopy'                                        , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopySunlitLAI)           = var_info('scalarCanopySunlitLAI'          , 'sunlit leaf area'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyShadedLAI)           = var_info('scalarCanopyShadedLAI'          , 'shaded leaf area'                                                 , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopySunlitPAR)           = var_info('scalarCanopySunlitPAR'          , 'average absorbed par for sunlit leaves'                           , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyShadedPAR)           = var_info('scalarCanopyShadedPAR'          , 'average absorbed par for shaded leaves'                           , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralBelowCanopyDirect)       = var_info('spectralBelowCanopyDirect'      , 'downward direct flux below veg layer for each spectral band'      , 'W m-2'           , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralBelowCanopyDiffuse)      = var_info('spectralBelowCanopyDiffuse'     , 'downward diffuse flux below veg layer for each spectral band'     , 'W m-2'           , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarBelowCanopySolar)          = var_info('scalarBelowCanopySolar'         , 'solar radiation transmitted below the canopy'                     , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralAlbGndDirect)            = var_info('spectralAlbGndDirect'           , 'direct  albedo of underlying surface for each spectral band'      , '-'               , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%spectralAlbGndDiffuse)           = var_info('spectralAlbGndDiffuse'          , 'diffuse albedo of underlying surface for each spectral band'      , '-'               , get_ixVarType('wLength'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundAlbedo)              = var_info('scalarGroundAlbedo'             , 'albedo of the ground surface'                                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyAbsorbedSolar)       = var_info('scalarCanopyAbsorbedSolar'      , 'solar radiation absorbed by canopy'                               , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundAbsorbedSolar)       = var_info('scalarGroundAbsorbedSolar'      , 'solar radiation absorbed by ground'                               , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (longwave radiation)
 mvar_meta(iLookMVAR%scalarCanopyEmissivity)          = var_info('scalarCanopyEmissivity'         , 'effective canopy emissivity'                                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadCanopy)               = var_info('scalarLWRadCanopy'              , 'longwave radiation emitted from the canopy'                       , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadGround)               = var_info('scalarLWRadGround'              , 'longwave radiation emitted at the ground surface'                 , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadUbound2Canopy)        = var_info('scalarLWRadUbound2Canopy'       , 'downward atmospheric longwave radiation absorbed by the canopy'   , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadUbound2Ground)        = var_info('scalarLWRadUbound2Ground'       , 'downward atmospheric longwave radiation absorbed by the ground'   , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadUbound2Ubound)        = var_info('scalarLWRadUbound2Ubound'       , 'atmospheric radiation refl by ground + lost thru upper boundary'  , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadCanopy2Ubound)        = var_info('scalarLWRadCanopy2Ubound'       , 'longwave radiation emitted from canopy lost thru upper boundary'  , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadCanopy2Ground)        = var_info('scalarLWRadCanopy2Ground'       , 'longwave radiation emitted from canopy absorbed by the ground'    , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadCanopy2Canopy)        = var_info('scalarLWRadCanopy2Canopy'       , 'canopy longwave reflected from ground and absorbed by the canopy' , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadGround2Ubound)        = var_info('scalarLWRadGround2Ubound'       , 'longwave radiation emitted from ground lost thru upper boundary'  , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWRadGround2Canopy)        = var_info('scalarLWRadGround2Canopy'       , 'longwave radiation emitted from ground and absorbed by the canopy', 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWNetCanopy)               = var_info('scalarLWNetCanopy'              , 'net longwave radiation at the canopy'                             , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWNetGround)               = var_info('scalarLWNetGround'              , 'net longwave radiation at the ground surface'                     , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLWNetUbound)               = var_info('scalarLWNetUbound'              , 'net longwave radiation at the upper atmospheric boundary'         , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (turbulent heat transfer)
 mvar_meta(iLookMVAR%scalarLatHeatSubVapCanopy)       = var_info('scalarLatHeatSubVapCanopy'      , 'latent heat of sublimation/vaporization used for veg canopy'      , 'J kg-1'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLatHeatSubVapGround)       = var_info('scalarLatHeatSubVapGround'      , 'latent heat of sublimation/vaporization used for ground surface'  , 'J kg-1'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSatVP_CanopyTemp)          = var_info('scalarSatVP_CanopyTemp'         , 'saturation vapor pressure at the temperature of vegetation canopy', 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSatVP_GroundTemp)          = var_info('scalarSatVP_GroundTemp'         , 'saturation vapor pressure at the temperature of the ground'       , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarZ0Canopy)                  = var_info('scalarZ0Canopy'                 , 'roughness length of the canopy'                                   , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarWindReductionFactor)       = var_info('scalarWindReductionFactor'      , 'canopy wind reduction factor'                                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarZeroPlaneDisplacement)     = var_info('scalarZeroPlaneDisplacement'    , 'zero plane displacement'                                          , 'm'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarRiBulkCanopy)              = var_info('scalarRiBulkCanopy'             , 'bulk Richardson number for the canopy'                            , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarRiBulkGround)              = var_info('scalarRiBulkGround'             , 'bulk Richardson number for the ground surface'                    , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyStabilityCorrection) = var_info('scalarCanopyStabilityCorrection', 'stability correction for the canopy'                              , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundStabilityCorrection) = var_info('scalarGroundStabilityCorrection', 'stability correction for the ground surface'                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarEddyDiffusCanopyTop)       = var_info('scalarEddyDiffusCanopyTop'      , 'eddy diffusivity for heat at the top of the canopy'               , 'm2 s-1'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarFrictionVelocity)          = var_info('scalarFrictionVelocity'         , 'friction velocity (canopy momentum sink)'                         , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarWindspdCanopyTop)          = var_info('scalarWindspdCanopyTop'         , 'windspeed at the top of the canopy'                               , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarWindspdCanopyBottom)       = var_info('scalarWindspdCanopyBottom'      , 'windspeed at the height of the bottom of the canopy'              , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundResistance)          = var_info('scalarGroundResistance'         , 'below canopy aerodynamic resistance'                              , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyResistance)          = var_info('scalarCanopyResistance'         , 'above canopy aerodynamic resistance'                              , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLeafResistance)            = var_info('scalarLeafResistance'           , 'mean leaf boundary layer resistance per unit leaf area'           , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilResistance)            = var_info('scalarSoilResistance'           , 'soil surface resistance'                                          , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilRelHumidity)           = var_info('scalarSoilRelHumidity'          , 'relative humidity in the soil pores in the upper-most soil layer' , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSenHeatTotal)              = var_info('scalarSenHeatTotal'             , 'sensible heat from the canopy air space to the atmosphere'        , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSenHeatCanopy)             = var_info('scalarSenHeatCanopy'            , 'sensible heat from the canopy to the canopy air space'            , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSenHeatGround)             = var_info('scalarSenHeatGround'            , 'sensible heat from the ground (below canopy or non-vegetated)'    , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLatHeatTotal)              = var_info('scalarLatHeatTotal'             , 'latent heat from the canopy air space to the atmosphere'          , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLatHeatCanopyEvap)         = var_info('scalarLatHeatCanopyEvap'        , 'evaporation latent heat from the canopy to the canopy air space'  , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLatHeatCanopyTrans)        = var_info('scalarLatHeatCanopyTrans'       , 'transpiration latent heat from the canopy to the canopy air space', 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLatHeatGround)             = var_info('scalarLatHeatGround'            , 'latent heat from the ground (below canopy or non-vegetated)'      , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyAdvectiveHeatFlux)   = var_info('scalarCanopyAdvectiveHeatFlux'  , 'heat advected to the canopy with precipitation (snow + rain)'     , 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundAdvectiveHeatFlux)   = var_info('scalarGroundAdvectiveHeatFlux'  , 'heat advected to the ground with throughfall + unloading/drainage', 'W m-2'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyTranspiration)       = var_info('scalarCanopyTranspiration'      , 'canopy transpiration'                                             , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyEvaporation)         = var_info('scalarCanopyEvaporation'        , 'canopy evaporation/condensation'                                  , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopySublimation)         = var_info('scalarCanopySublimation'        , 'canopy sublimation/frost'                                         , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundEvaporation)         = var_info('scalarGroundEvaporation'        , 'ground evaporation/condensation (below canopy or non-vegetated)'  , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSnowSublimation)           = var_info('scalarSnowSublimation'          , 'snow sublimation/frost (below canopy or non-vegetated)'           , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (transpiration)
 mvar_meta(iLookMVAR%scalarTranspireLim)              = var_info('scalarTranspireLim'             , 'aggregate soil moisture and aquifer control on transpiration'     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarTranspireLimAqfr)          = var_info('scalarTranspireLimAqfr'         , 'aquifer storage control on transpiration'                         , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarFoliageNitrogenFactor)     = var_info('scalarFoliageNitrogenFactor'    , 'foliage nitrogen concentration (1=saturated)'                     , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarStomResistSunlit)          = var_info('scalarStomResistSunlit'         , 'stomatal resistance for sunlit leaves'                            , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarStomResistShaded)          = var_info('scalarStomResistShaded'         , 'stomatal resistance for shaded leaves'                            , 's m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarPhotosynthesisSunlit)      = var_info('scalarPhotosynthesisSunlit'     , 'sunlit photosynthesis'                                            , 'umolco2 m-2 s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarPhotosynthesisShaded)      = var_info('scalarPhotosynthesisShaded'     , 'shaded photosynthesis'                                            , 'umolco2 m-2 s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarIntercellularCO2Sunlit)    = var_info('scalarIntercellularCO2Sunlit'   , 'carbon dioxide partial pressure of leaf interior (sunlit leaves)' , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarIntercellularCO2Shaded)    = var_info('scalarIntercellularCO2Shaded'   , 'carbon dioxide partial pressure of leaf interior (shaded leaves)' , 'Pa'              , get_ixVarType('scalarv'), integerMissing, 0)
 ! vegetation variables (canopy water)
 mvar_meta(iLookMVAR%scalarCanopyWetFraction)         = var_info('scalarCanopyWetFraction'        , 'fraction canopy that is wet'                                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarGroundSnowFraction)        = var_info('scalarGroundSnowFraction'       , 'fraction ground that is covered with snow'                        , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarThroughfallSnow)           = var_info('scalarThroughfallSnow'          , 'snow that reaches the ground without ever touching the canopy'    , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarThroughfallRain)           = var_info('scalarThroughfallRain'          , 'rain that reaches the ground without ever touching the canopy'    , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopySnowUnloading)       = var_info('scalarCanopySnowUnloading'      , 'unloading of snow from the vegetation canopy'                     , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyLiqDrainage)         = var_info('scalarCanopyLiqDrainage'        , 'drainage of liquid water from the vegetation canopy'              , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarCanopyMeltFreeze)          = var_info('scalarCanopyMeltFreeze'         , 'melt/freeze of water stored in the canopy'                        , 'kg m-2 s-1'      , get_ixVarType('scalarv'), integerMissing, 0)
 ! scalar variables (soil and aquifer fluxes)
 mvar_meta(iLookMVAR%scalarRainPlusMelt)              = var_info('scalarRainPlusMelt'             , 'rain plus melt, used as input to soil before surface runoff'      , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarInfilArea)                 = var_info('scalarInfilArea'                , 'fraction of unfrozen area where water can infiltrate'             , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarFrozenArea)                = var_info('scalarFrozenArea'               , 'fraction of area that is considered impermeable due to soil ice'  , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarInfiltration)              = var_info('scalarInfiltration'             , 'infiltration of water into the soil profile'                      , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarExfiltration)              = var_info('scalarExfiltration'             , 'exfiltration of water from the top of the soil profile'           , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSurfaceRunoff)             = var_info('scalarSurfaceRunoff'            , 'surface runoff'                                                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarInitAquiferRecharge)       = var_info('scalarInitAquiferRecharge'      , 'recharge to the aquifer at the start-of-step'                     , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferRecharge)           = var_info('scalarAquiferRecharge'          , 'recharge to the aquifer at the end-of-step'                       , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarInitAquiferTranspire)      = var_info('scalarInitAquiferTranspire'     , 'transpiration loss from the aquifer at the start-of-step'         , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferTranspire)          = var_info('scalarAquiferTranspire'         , 'transpiration loss from the aquifer at the end-of-step'           , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarInitAquiferBaseflow)       = var_info('scalarInitAquiferBaseflow'      , 'baseflow from the aquifer at the start-of-step'                   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferBaseflow)           = var_info('scalarAquiferBaseflow'          , 'baseflow from the aquifer at the end-of-step'                     , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 ! scalar variables (sub-step average fluxes for the soil zone)
 mvar_meta(iLookMVAR%scalarSoilInflux)                = var_info('scalarSoilInflux'               , 'sub-step average: influx of water at the top of the soil profile' , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilCompress)              = var_info('scalarSoilCompress'             , 'change in total soil storage due to compression of soil matrix'   , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilBaseflow)              = var_info('scalarSoilBaseflow'             , 'sub-step average: total baseflow from the soil profile'           , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilDrainage)              = var_info('scalarSoilDrainage'             , 'sub-step average: drainage from the bottom of the soil profile'   , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarSoilTranspiration)         = var_info('scalarSoilTranspiration'        , 'sub-step average: total transpiration from the soil'              , 'm s-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 ! scalar variables (mass balance check)
 mvar_meta(iLookMVAR%scalarSoilWatBalError)           = var_info('scalarSoilWatBalError'          , 'error in the total soil water balance'                            , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferBalError)           = var_info('scalarAquiferBalError'          , 'error in the aquifer water balance'                               , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarTotalSoilLiq)              = var_info('scalarTotalSoilLiq'             , 'total mass of liquid water in the soil'                           , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarTotalSoilIce)              = var_info('scalarTotalSoilIce'             , 'total mass of ice in the soil'                                    , 'kg m-2'          , get_ixVarType('scalarv'), integerMissing, 0)
 ! variables at the mid-point of each layer -- domain geometry
 mvar_meta(iLookMVAR%mLayerDepth)                     = var_info('mLayerDepth'                    , 'depth of each layer'                                              , 'm'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerHeight)                    = var_info('mLayerHeight'                   , 'height of the layer mid-point (top of soil = 0)'                  , 'm'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerRootDensity)               = var_info('mLayerRootDensity'              , 'fraction of roots in each soil layer'                             , '-'               , get_ixVarType('midSoil'), integerMissing, 0)
 ! variables at the mid-point of each layer coupled energy and mass
 mvar_meta(iLookMVAR%mLayerTemp)                      = var_info('mLayerTemp'                     , 'temperature of each layer'                                        , 'K'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerVolFracAir)                = var_info('mLayerVolFracAir'               , 'volumetric fraction of air in each layer'                         , '-'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerVolFracIce)                = var_info('mLayerVolFracIce'               , 'volumetric fraction of ice in each layer'                         , '-'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerVolFracLiq)                = var_info('mLayerVolFracLiq'               , 'volumetric fraction of liquid water in each layer'                , '-'               , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerVolHtCapBulk)              = var_info('mLayerVolHtCapBulk'             , 'volumetric heat capacity in each layer'                           , 'J m-3 K-1'       , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerTcrit)                     = var_info('mLayerTcrit'                    , 'critical soil temperature above which all water is unfrozen'      , 'K'               , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerdTheta_dTk)                = var_info('mLayerdTheta_dTk'               , 'derivative in volumetric liquid water content wrt temperature'    , 'K-1'             , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerThermalC)                  = var_info('mLayerThermalC'                 , 'thermal conductivity at the mid-point of each layer'              , 'W m-1 K-1'       , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerRadCondFlux)               = var_info('mLayerRadCondFlux'              , 'temporal derivative in energy of radiative and conductive flux'   , 'J m-3 s-1'       , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerMeltFreeze)                = var_info('mLayerMeltFreeze'               , 'ice content change from melt/freeze in each layer'                , 'kg m-3'          , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerInfilFreeze)               = var_info('mLayerInfilFreeze'              , 'ice content change by freezing infiltrating flux'                 , 'kg m-3'          , get_ixVarType('midToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerSatHydCond)                = var_info('mLayerSatHydCond'               , 'saturated hydraulic conductivity in each layer'                   , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerSatHydCondMP)              = var_info('mLayerSatHydCondMP'             , 'saturated hydraulic conductivity of macropores in each layer'     , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerMatricHead)                = var_info('mLayerMatricHead'               , 'matric head of water in the soil'                                 , 'm'               , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerdTheta_dPsi)               = var_info('mLayerdTheta_dPsi'              , 'derivative in the soil water characteristic w.r.t. psi'           , 'm-1'             , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerdPsi_dTheta)               = var_info('mLayerdPsi_dTheta'              , 'derivative in the soil water characteristic w.r.t. theta'         , 'm'               , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerThetaResid)                = var_info('mLayerThetaResid'               , 'residual volumetric water content in each snow layer'             , '-'               , get_ixVarType('midSnow'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerPoreSpace)                 = var_info('mLayerPoreSpace'                , 'total pore space in each snow layer'                              , '-'               , get_ixVarType('midSnow'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerCompress)                  = var_info('mLayerCompress'                 , 'change in volumetric water content due to compression of soil'    , '-'               , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerTranspireLim)              = var_info('mLayerTranspireLim'             , 'soil moist & veg limit on transpiration for each layer'           , '-'               , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerInitTranspire)             = var_info('mLayerInitTranspire'            , 'transpiration loss from each soil layer at the start-of-step'     , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerTranspire)                 = var_info('mLayerTranspire'                , 'transpiration loss from each soil layer'                          , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerInitQMacropore)            = var_info('mLayerInitQMacropore'           , 'liquid flux from micropores to macropores at the start-of-step'   , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerQMacropore)                = var_info('mLayerQMacropore'               , 'liquid flux from micropores to macropores'                        , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerInitBaseflow)              = var_info('mLayerInitBaseflow'             , 'baseflow from each soil layer at the start of the time step'      , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerBaseflow)                  = var_info('mLayerBaseflow'                 , 'baseflow from each soil layer'                                    , 'm s-1'           , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerColumnInflow)              = var_info('mLayerColumnInflow'             , 'total inflow to each layer in a given soil column'                , 'm3 s-1'          , get_ixVarType('midSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%mLayerColumnOutflow)             = var_info('mLayerColumnOutflow'            , 'total outflow from each layer in a given soil column'             , 'm3 s-1'          , get_ixVarType('midSoil'), integerMissing, 0)
 ! variables at the interface of each layer
 mvar_meta(iLookMVAR%iLayerHeight)                    = var_info('iLayerHeight'                   , 'height of the layer interface (top of soil = 0)'                  , 'm'               , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerThermalC)                  = var_info('iLayerThermalC'                 , 'thermal conductivity at the interface of each layer'              , 'W m-1 K-1'       , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerConductiveFlux)            = var_info('iLayerConductiveFlux'           , 'conductive energy flux at layer interfaces at end of time step'   , 'W m-2'           , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerAdvectiveFlux)             = var_info('iLayerAdvectiveFlux'            , 'advective energy flux at layer interfaces at end of time step'    , 'W m-2'           , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerInitNrgFlux)               = var_info('iLayerInitNrgFlux'              , 'energy flux at layer interfaces at the start of the time step'    , 'W m-2'           , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerNrgFlux)                   = var_info('iLayerNrgFlux'                  , 'energy flux at layer interfaces at end of the time step'          , 'W m-2'           , get_ixVarType('ifcToto'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerSatHydCond)                = var_info('iLayerSatHydCond'               , 'saturated hydraulic conductivity in each layer interface'         , 'm s-1'           , get_ixVarType('ifcSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerInitLiqFluxSnow)           = var_info('iLayerInitLiqFluxSnow'          , 'liquid flux at snow layer interfaces at start of the time step'   , 'm s-1'           , get_ixVarType('ifcSnow'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerInitLiqFluxSoil)           = var_info('iLayerInitLiqFluxSoil'          , 'liquid flux at soil layer interfaces at start of the time step'   , 'm s-1'           , get_ixVarType('ifcSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerInitFluxReversal)          = var_info('iLayerInitFluxReversal'         , 'start of step liquid flux at soil layer interfaces from impedance', 'm s-1'           , get_ixVarType('ifcSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerLiqFluxSnow)               = var_info('iLayerLiqFluxSnow'              , 'liquid flux at snow layer interfaces at end of the time step'     , 'm s-1'           , get_ixVarType('ifcSnow'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerLiqFluxSoil)               = var_info('iLayerLiqFluxSoil'              , 'liquid flux at soil layer interfaces at end of the time step'     , 'm s-1'           , get_ixVarType('ifcSoil'), integerMissing, 0)
 mvar_meta(iLookMVAR%iLayerFluxReversal)              = var_info('iLayerFluxReversal'             , 'end of step liquid flux at soil layer interfaces from impedance'  , 'm s-1'           , get_ixVarType('ifcSoil'), integerMissing, 0)
 ! time steppin,
 mvar_meta(iLookMVAR%dt_init)                         = var_info('dt_init'                        , 'length of initial time step at start of next data interval'       , 's'               , get_ixVarType('scalarv'), integerMissing, 0)
 ! "short-cut" variables
 mvar_meta(iLookMVAR%scalarVGn_m)                     = var_info('scalarVGn_m'                    , 'van Genuchten "m" parameter'                                      , '-'               , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarKappa)                     = var_info('scalarKappa'                    , 'constant in the freezing curve function'                          , 'm K-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVolHtCap_air)              = var_info('scalarVolHtCap_air'             , 'volumetric heat capacity air'                                     , 'J m-3 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVolHtCap_ice)              = var_info('scalarVolHtCap_ice'             , 'volumetric heat capacity ice'                                     , 'J m-3 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVolHtCap_soil)             = var_info('scalarVolHtCap_soil'            , 'volumetric heat capacity dry soil'                                , 'J m-3 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVolHtCap_water)            = var_info('scalarVolHtCap_water'           , 'volumetric heat capacity liquid wat'                              , 'J m-3 K-1'       , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLambda_drysoil)            = var_info('scalarLambda_drysoil'           , 'thermal conductivity of dry soil'                                 , 'W m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarLambda_wetsoil)            = var_info('scalarLambda_wetsoil'           , 'thermal conductivity of wet soil'                                 , 'W m-1'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarVolLatHt_fus)              = var_info('scalarVolLatHt_fus'             , 'volumetric latent heat of fusion'                                 , 'J m-3'           , get_ixVarType('scalarv'), integerMissing, 0)
 mvar_meta(iLookMVAR%scalarAquiferRootFrac)           = var_info('scalarAquiferRootFrac'          , 'fraction of roots below the soil profile (in the aquifer)'        , '-'               , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * basin-wide runoff and aquifer fluxes...
 ! -----------------------------------------
 bvar_meta(iLookBVAR%basin__totalArea)        = var_info('basin__totalArea'       , 'total basin area'                                       , 'm2'    , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__SurfaceRunoff)    = var_info('basin__SurfaceRunoff'   , 'surface runoff'                                         , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__ColumnOutflow)    = var_info('basin__ColumnOutflow'   , 'outflow from all "outlet" HRUs (with no downstream HRU)', 'm3 s-1', get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__AquiferStorage)   = var_info('basin__AquiferStorage'  , 'aquifer storage'                                        , 'm'     , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__AquiferRecharge)  = var_info('basin__AquiferRecharge' , 'recharge to the aquifer'                                , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__AquiferBaseflow)  = var_info('basin__AquiferBaseflow' , 'baseflow from the aquifer'                              , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%basin__AquiferTranspire) = var_info('basin__AquiferTranspire', 'transpiration loss from the aquifer'                    , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%routingRunoffFuture)     = var_info('routingRunoffFuture'    , 'runoff in future time steps'                            , 'm s-1' , get_ixVarType('routing'), integerMissing, 0)
 bvar_meta(iLookBVAR%routingFractionFuture)   = var_info('routingFractionFuture'  , 'fraction of runoff in future time steps'                , '-'     , get_ixVarType('routing'), integerMissing, 0)
 bvar_meta(iLookBVAR%averageInstantRunoff)    = var_info('averageInstantRunoff'   , 'instantaneous runoff'                                   , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)
 bvar_meta(iLookBVAR%averageRoutedRunoff)     = var_info('averageRoutedRunoff'    , 'routed runoff'                                          , 'm s-1' , get_ixVarType('scalarv'), integerMissing, 0)

 ! -----
 ! * run time indices...
 ! ---------------------
 indx_meta(iLookINDEX%nSnow)             = var_info('nSnow'            , 'number of snow layers'                                 , '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%nSoil)             = var_info('nSoil'            , 'number of soil layers'                                 , '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%nLayers)           = var_info('nLayers'          , 'total number of layers'                                , '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%midSnowStartIndex) = var_info('midSnowStartIndex', 'start index of the midSnow vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%midSoilStartIndex) = var_info('midSoilStartIndex', 'start index of the midSoil vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%midTotoStartIndex) = var_info('midTotoStartIndex', 'start index of the midToto vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%ifcSnowStartIndex) = var_info('ifcSnowStartIndex', 'start index of the ifcSnow vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%ifcSoilStartIndex) = var_info('ifcSoilStartIndex', 'start index of the ifcSoil vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%ifcTotoStartIndex) = var_info('ifcTotoStartIndex', 'start index of the ifcToto vector for a given timestep', '-', get_ixVarType('scalarv'), integerMissing, 0)
 indx_meta(iLookINDEX%layerType)         = var_info('layerType'        , 'index defining type of layer (soil or snow)'           , '-', get_ixVarType('midToto'), integerMissing, 0)

 ! -----
 ! * call routine to read the user control file specifying desired model outputs
 ! -----------------------------------------
 call read_output_file(err,cmessage) 
 message=trim(message)//trim(cmessage) 

 end subroutine popMetadat

! ------------------------------------------------
! subroutine to populate write commands from file input
! ------------------------------------------------
 subroutine read_output_file(err,message)
 USE nrtype
 USE multiconst,only:integerMissing
 ! data structures
 USE data_struc, only: var_info   ! data type for metadata structure
 USE data_struc, only: time_meta  ! data structure for time metadata
 USE data_struc, only: forc_meta  ! data structure for forcing metadata
 USE data_struc, only: type_meta  ! data structure for categorical metadata
 USE data_struc, only: attr_meta  ! data structure for attribute metadata
 USE data_struc, only: mpar_meta  ! data structure for local parameter metadata
 USE data_struc, only: bpar_meta  ! data structure for basin parameter metadata
 USE data_struc, only: mvar_meta  ! data structure for local model variable metadata
 USE data_struc, only: bvar_meta  ! data structure for basin model variable metadata
 USE data_struc, only: indx_meta  ! data structure for index metadata
 ! structures of named variables
 USE var_lookup, only: iLookTIME  ! named variables for time data structure
 USE var_lookup, only: iLookFORCE ! named variables for forcing data structure
 USE var_lookup, only: iLookTYPE  ! named variables for categorical attribute data structure
 USE var_lookup, only: iLookATTR  ! named variables for real valued attribute data structure
 USE var_lookup, only: iLookPARAM ! named variables for local parameter data structure
 USE var_lookup, only: iLookBPAR  ! named variables for basin parameter data structure
 USE var_lookup, only: iLookMVAR  ! named variables for local model variable data structure
 USE var_lookup, only: iLookBVAR  ! named variables for basin model variable data structure
 USE var_lookup, only: iLookINDEX ! named variables for index variable data structure
 USE var_lookup, only: iLookSTAT  ! named variables for output statistics data structure
 USE var_lookup, only: iLookVarType  ! named variables for variable type data structure
 ! get routines for indexing into variable types
 USE get_ixname_module,only:get_ixTime  ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixAttr  ! to retreive variable index from variable name  
 USE get_ixname_module,only:get_ixType  ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixForce ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixParam ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixMvar  ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixIndex ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixBpar  ! to retreive variable index from variable name 
 USE get_ixname_module,only:get_ixBvar  ! to retreive variable index from variable name 
 ! to get name of output control file from user
 USE summaFileManager,only:SETNGS_PATH     ! path for metadata files
 USE summaFileManager,only:META_LOCALMVAR  ! file with output controls
 ! modules for smart file reading
 USE ascii_util_module,only:get_vlines      ! get a vector of non-comment lines
 USE ascii_util_module,only:file_open       ! open file
 USE ascii_util_module,only:split_line      ! split a line into words
 implicit none
 ! dummy variables
 integer(i4b),intent(out)       :: err             ! error code
 character(*),intent(out)       :: message         ! error message
 ! local variables
 character(LEN=256)             :: cmessage        ! error message of downwind routine
 character(LEN=256)             :: outfile         ! full path of model output file 
 integer(i4b)                   :: unt             ! file unit
 character(LEN=512),allocatable :: charlines(:)    ! vector of character strings
 character(LEN=64),allocatable  :: lineWords(:)    ! vector to parse textline
 integer(i4b)                   :: frequency       ! output frequency
 integer(i4b)                   :: statistic       ! output frequency
 ! indices
 integer(i4b)                   :: vLine           ! index for loop through variables
 integer(i4b)                   :: vDex            ! index into type lists
 ! flags
 logical(lgt)                   :: midSnow ! logical flags to turn on index variables if they are not requested
 logical(lgt)                   :: midSoil ! logical flags to turn on index variables if they are not requested
 logical(lgt)                   :: midToto ! logical flags to turn on index variables if they are not requested
 logical(lgt)                   :: ifcSnow ! logical flags to turn on index variables if they are not requested
 logical(lgt)                   :: ifcSoil ! logical flags to turn on index variables if they are not requested
 logical(lgt)                   :: ifcToto ! logical flags to turn on index variables if they are not requested
 integer(i4b)                   :: oFreqLayered ! output frequency for layered variables
 ! initialize error control
 err=0; message='read_output_file/'

 ! **********************************************************************************************
 ! (1) open file and read variable data
 ! **********************************************************************************************
 outfile = trim(SETNGS_PATH)//trim(META_LOCALMVAR)   ! build filename
print*,'Name of Model Output control file: ',trim(outfile) ! GERY
 call file_open(trim(outfile),unt,err,cmessage)      ! open file
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif

 ! **********************************************************************************************
 ! (2) read variable data (continue reading from previous point in the file)
 ! **********************************************************************************************
 ! read the master frequency of layered variables
 read(unt,*) oFreqLayered
 call get_vlines(unt,charLines,err,cmessage) ! get a list of character strings from non-comment lines
 if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
 close(unt) ! close the file 

 ! **********************************************************************************************
 ! (3) loop to parse individual file lines 
 ! **********************************************************************************************
 ! flag whether or not the user has requested an output variable that requires output of layer information
 midSnow = .false.
 midSoil = .false.
 midToto = .false.
 ifcSnow = .false.
 ifcSoil = .false.
 ifcToto = .false.

 ! user does not have the ability to change the time output (needs to change GREY)
 forc_meta(iLookForce%time)%o_freq                 = 0
 forc_meta(iLookForce%time)%o_freq(iLookStat%inst) = 1

 ! loop through the lines in the file
 do vLine = 1,size(charLines) 
  ! parse current line  
  call split_line(charLines(vLine),lineWords,err,cmessage)
  if(err/=0)then; message=trim(message)//trim(cmessage); return; endif
  ! varname = lineWords(1), statistic = linewords(3), frequency = linewords(5)
  ! the rest of the line is not important

  ! turn the stat and frequency into integers
  read(linewords(3),'(i1)') statistic
  read(linewords(5),'(i1)') frequency

  ! check for dummy entries
  if (statistic.eq.0) cycle                          ! do nothing if the statistic is not a positive integer 
  if (trim(lineWords(1)).eq.'time') cycle            ! user does not have control over time output (needs to change GREY)

  ! assign values to appropriate structure
  vDex = get_ixMvar(trim(lineWords(1)))
  if (vDex.gt.0) then
   mvar_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(mvar_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     mvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  ! assign values to appropriate structure
  vDex = get_ixTime(trim(lineWords(1)))
  if (vDex.gt.0) then
   time_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(time_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     time_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      time_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixForce(trim(lineWords(1)))
  if (vDex.gt.0) then
   forc_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(forc_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     forc_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      forc_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixAttr(trim(lineWords(1)))
  if (vDex.gt.0) then
   attr_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(attr_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     attr_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      attr_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixType(trim(lineWords(1)))
  if (vDex.gt.0) then
   type_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(type_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     type_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      type_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixParam(trim(lineWords(1)))
  if (vDex.gt.0) then
   mpar_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(mpar_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     mpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      mpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixIndex(trim(lineWords(1)))
  if (vDex.gt.0) then
   indx_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(indx_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     indx_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      indx_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixBpar(trim(lineWords(1)))
  if (vDex.gt.0) then
   bpar_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(bpar_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     bpar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bpar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  vDex = get_ixBvar(trim(lineWords(1)))
  if (vDex.gt.0) then
   bvar_meta(vDex)%o_freq(statistic)  = frequency   ! non-layer varaibles are turned on according to file instructions
   selectcase(bvar_meta(vDex)%vartype)              ! check wither this is one of the layer-type variables
    case (iLookVarType%midSnow)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midSoil)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%midToto)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      midToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSnow)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSnow = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcSoil)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcSoil = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    case (iLookVarType%ifcToto)                     ! if it is a layer-type variable, do the following: 
     bvar_meta(vDex)%o_freq(statistic)  = 0         ! turn all write flags off if this is a layer-type variable
     if (statistic.eq.iLookStat%inst) then          ! only turn on a layer-type variable in instantaneous mode
      ifcToto = .true.                              ! turn on the flag to ensure output of the appropriate index variable
      bvar_meta(vDex)%o_freq(iLookStat%inst) = oFreqLayered ! turn on the variable and force the frequency to be timestep
     endif
    endselect ! variable type
   cycle ! go to next variable in file
  endif ! vDex found

  ! if we get this far then the variable was not found
  err=20; message=trim(message)//'variable not found='; message=trim(message)//trim(lineWords(1)); return;
 enddo ! vLine

 if (midSnow) then
  indx_meta(iLookINDEX%midSnowStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nSnow)%o_freq(iLookStat%inst)             = 1
 endif
 if (midSoil) then
  indx_meta(iLookINDEX%midSoilStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nSoil)%o_freq(iLookStat%inst)             = 1
 endif
 if (midToto) then
  indx_meta(iLookINDEX%midTotoStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nLayers)%o_freq(iLookStat%inst)           = 1
 endif
 if (ifcSnow) then
  indx_meta(iLookINDEX%ifcSnowStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nSnow)%o_freq(iLookStat%inst)             = 1
 endif
 if (ifcSoil) then
  indx_meta(iLookINDEX%ifcSoilStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nSoil)%o_freq(iLookStat%inst)             = 1
 endif
 if (ifcToto) then
  indx_meta(iLookINDEX%ifcTotoStartIndex)%o_freq(iLookStat%inst) = 1
  indx_meta(iLookINDEX%nLayers)%o_freq(iLookStat%inst)           = 1
 endif

 return
 end subroutine read_output_file

end module popMetadat_module
