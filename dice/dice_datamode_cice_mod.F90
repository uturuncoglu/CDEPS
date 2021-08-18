module dice_datamode_cice_mod

  use ESMF                 , only : ESMF_State, ESMF_LogWrite, ESMF_Array, ESMF_MeshGet
  use ESMF                 , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO, ESMF_DistGrid
  use ESMF                 , only : ESMF_ArrayCreate, ESMF_ArrayDestroy
  use NUOPC                , only : NUOPC_Advertise
  use shr_kind_mod         , only : r8=>shr_kind_r8
  use dshr_strdata_mod     , only : shr_strdata_get_stream_pointer, shr_strdata_type
  use dshr_methods_mod     , only : dshr_state_getfldptr, chkerr
  use dshr_mod             , only : dshr_restart_read, dshr_restart_write
  use dshr_fldlist_mod     , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public  :: dice_datamode_cice_advertise
  public  :: dice_datamode_cice_init_pointers
  public  :: dice_datamode_cice_advance
  public  :: dice_datamode_cice_restart_write
  public  :: dice_datamode_cice_restart_read

  ! restart fields
  real(r8), pointer, public :: water(:) => null()

  ! internal fields
  real(r8), pointer :: yc(:)      => null() ! mesh lats (degrees)
  integer , pointer :: imask(:)   => null()

  ! export fields
  real(r8), pointer :: Si_imask(:)       => null()
  real(r8), pointer :: Si_ifrac(:)       => null()
  real(r8), pointer :: Si_t(:)           => null()
  real(r8), pointer :: Si_vice(:)        => null()
  real(r8), pointer :: Si_vsno(:)        => null()
  real(r8), pointer :: Si_avsdr(:)       => null()
  real(r8), pointer :: Si_avsdf(:)       => null()
  real(r8), pointer :: Si_anidr(:)       => null()
  real(r8), pointer :: Si_anidf(:)       => null()
  real(r8), pointer :: Faii_sen(:)       => null()
  real(r8), pointer :: Faii_lat(:)       => null()
  real(r8), pointer :: Faii_lwup(:)      => null()
  real(r8), pointer :: Faii_evap(:)      => null()
  real(r8), pointer :: Faii_taux(:)      => null()
  real(r8), pointer :: Faii_tauy(:)      => null()
  real(r8), pointer :: Fioi_melth(:)     => null()
  real(r8), pointer :: Fioi_meltw(:)     => null()
  real(r8), pointer :: Fioi_swpen(:)     => null()
  real(r8), pointer :: Fioi_swpen_vdr(:) => null()
  real(r8), pointer :: Fioi_swpen_vdf(:) => null()
  real(r8), pointer :: Fioi_swpen_idr(:) => null()
  real(r8), pointer :: Fioi_swpen_idf(:) => null()
  real(r8), pointer :: Fioi_taux(:)      => null()
  real(r8), pointer :: Fioi_tauy(:)      => null()
  real(r8), pointer :: Fioi_salt(:)      => null()

  ! other parameters
  character(*) , parameter :: nullstr = 'null'
  character(*) , parameter :: rpfile  = 'rpointer.ice'
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dice_datamode_cice_advertise(importState, exportState, fldsimport, fldsexport, &
       flds_scalar_name, rc)

    ! --------------------------------------------------------------
    ! determine export and import fields to advertise to mediator
    ! --------------------------------------------------------------

    ! input/output arguments
    type(ESMF_State)  , intent(inout) :: importState
    type(ESMF_State)  , intent(inout) :: exportState
    type(fldlist_type), pointer       :: fldsimport
    type(fldlist_type), pointer       :: fldsexport
    character(len=*)  , intent(in)    :: flds_scalar_name
    integer           , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    ! Advertise export fields
    call dshr_fldList_add(fldsExport , trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport ,'Si_ifrac'      )
    call dshr_fldList_add(fldsExport ,'Si_imask'      )
    call dshr_fldList_add(fldsExport ,'Si_t'          )
    call dshr_fldList_add(fldsExport ,'Si_vice'       )
    call dshr_fldList_add(fldsExport ,'Si_vsno'       )
    call dshr_fldList_add(fldsExport ,'Si_avsdr'      )
    call dshr_fldList_add(fldsExport ,'Si_avsdf'      )
    call dshr_fldList_add(fldsExport ,'Si_anidr'      )
    call dshr_fldList_add(fldsExport ,'Si_anidf'      )
    call dshr_fldList_add(fldsExport ,'Faii_sen'      )
    call dshr_fldList_add(fldsExport ,'Faii_lat'      )
    call dshr_fldList_add(fldsExport ,'Faii_lwup'     )
    call dshr_fldList_add(fldsExport ,'Faii_evap'     )
    call dshr_fldList_add(fldsExport ,'Faii_taux'     )
    call dshr_fldList_add(fldsExport ,'Faii_tauy'     )
    call dshr_fldList_add(fldsExport ,'Fioi_melth'    )
    call dshr_fldList_add(fldsExport ,'Fioi_meltw'    )
    call dshr_fldList_add(fldsExport ,'Fioi_swpen'    )
    call dshr_fldList_add(fldsExport ,'Fioi_swpen_vdr')
    call dshr_fldList_add(fldsExport ,'Fioi_swpen_vdf')
    call dshr_fldList_add(fldsExport ,'Fioi_swpen_idr')
    call dshr_fldList_add(fldsExport ,'Fioi_swpen_idf')
    call dshr_fldList_add(fldsExport ,'Fioi_taux'     )
    call dshr_fldList_add(fldsExport ,'Fioi_tauy'     )
    call dshr_fldList_add(fldsExport ,'Fioi_salt'     )

    ! Advertise import fields
    ! N/A

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dice_comp_advertise): Fr_ice'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine dice_datamode_cice_advertise

  !===============================================================================
  subroutine dice_datamode_cice_init_pointers(importState, exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: importState
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    integer             :: n
    integer             :: lsize
    type(ESMF_DistGrid) :: distGrid
    type(ESMF_Array)    :: elemMaskArray
    integer             :: spatialDim         ! number of dimension in mesh
    integer             :: numOwnedElements   ! size of mesh
    real(r8), pointer   :: ownedElemCoords(:) ! mesh lat and lons
    character(len=*), parameter :: subname='(dice_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = sdat%model_lsize

    ! Set Si_imask (this corresponds to the ocean mask)
    call dshr_state_getfldptr(exportState, fldname='Si_imask'    , fldptr1=Si_imask    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(imask(sdat%model_lsize))
    call ESMF_MeshGet(sdat%model_mesh, numOwnedElements=numOwnedElements, elementdistGrid=distGrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    elemMaskArray = ESMF_ArrayCreate(distGrid, imask, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_MeshGet(sdat%model_mesh, elemMaskArray=elemMaskArray, rc=rc) ! set the varues of imask
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    Si_imask(:) = real(imask(:), kind=r8) ! set the mask as real
    call ESMF_ArrayDestroy(elemMaskArray, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set export state module pointers
    call dshr_state_getfldptr(exportState, fldname='Si_ifrac'      , fldptr1=Si_ifrac      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_t'          , fldptr1=Si_t          , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_vice'       , fldptr1=Si_vice       , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_vsno'       , fldptr1=Si_vsno       , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_avsdr'      , fldptr1=Si_avsdr      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_avsdf'      , fldptr1=Si_avsdf      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_anidr'      , fldptr1=Si_anidr      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Si_anidf'      , fldptr1=Si_anidf      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_sen'      , fldptr1=Faii_sen      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_lat'      , fldptr1=Faii_lat      , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_lwup'     , fldptr1=Faii_lwup     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_evap'     , fldptr1=Faii_evap     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_taux'     , fldptr1=Faii_taux     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Faii_tauy'     , fldptr1=Faii_tauy     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_melth'    , fldptr1=Fioi_melth    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_meltw'    , fldptr1=Fioi_meltw    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_swpen'    , fldptr1=Fioi_swpen    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_swpen_vdr', fldptr1=Fioi_swpen_vdr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_swpen_vdf', fldptr1=Fioi_swpen_vdf, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_swpen_idr', fldptr1=Fioi_swpen_idr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_swpen_idf', fldptr1=Fioi_swpen_idf, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_taux'     , fldptr1=Fioi_taux     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_tauy'     , fldptr1=Fioi_tauy     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, fldname='Fioi_salt'     , fldptr1=Fioi_salt     , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Set pointers to importState fields
    ! N/A

    ! Determine model latitudes
    allocate(yc(lsize))
    call ESMF_MeshGet(sdat%model_mesh, spatialDim=spatialDim, numOwnedElements=numOwnedElements, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(ownedElemCoords(spatialDim*numOwnedElements))
    call ESMF_MeshGet(sdat%model_mesh, ownedElemCoords=ownedElemCoords)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do n = 1,numOwnedElements
       yc(n) = ownedElemCoords(2*n)
    end do
    deallocate(ownedElemCoords)

  end subroutine dice_datamode_cice_init_pointers

  !===============================================================================
  subroutine dice_datamode_cice_advance(exportState, importState, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(ESMF_State)       , intent(inout) :: importState
    integer                , intent(out)   :: rc

    ! local variables
    integer               :: n
    integer               :: lsize
    character(len=*), parameter :: subname='(dice_datamode_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lsize = size(Si_ifrac)
    do n = 1, lsize
      !--- fix erroneous iFrac ---
      Si_ifrac(n) = min(1.0_r8,max(0.0_r8,Si_ifrac(n)))

      ! multiply ice and snow thickness (m) with ice area to 
      ! calculate ice and snow volume (m^3)
      Si_vice(n) = Si_vice(n)*Si_ifrac(n)
      Si_vsno(n) = Si_vsno(n)*Si_ifrac(n)

      ! calculate SW penetration thru the ice in bands
      ! simply distribute with simple weights
      ! inherited from how GFDL was driving MOM6 with the CORE2 forcings
      Fioi_swpen_vdr(n) = Fioi_swpen(n)*0.285_r8
      Fioi_swpen_vdf(n) = Fioi_swpen(n)*0.285_r8
      Fioi_swpen_idr(n) = Fioi_swpen(n)*0.215_r8
      Fioi_swpen_idf(n) = Fioi_swpen(n)*0.215_r8
    end do

  end subroutine dice_datamode_cice_advance

  !===============================================================================
  subroutine dice_datamode_cice_restart_write(case_name, inst_suffix, ymd, tod, &
       logunit, my_task, sdat)

    ! input/output variables
    character(len=*)            , intent(in)    :: case_name
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: ymd       ! model date
    integer                     , intent(in)    :: tod       ! model sec into model date
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    !call dshr_restart_write(rpfile, case_name, 'dice', inst_suffix, ymd, tod, &
    !     logunit, my_task, sdat, fld=water, fldname='water')

  end subroutine dice_datamode_cice_restart_write

  !===============================================================================
  subroutine dice_datamode_cice_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    ! allocate module memory for restart fields that are read in
    allocate(water(sdat%model_lsize))

    ! read restart
    !call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat, &
    !     fld=water, fldname='water')

  end subroutine dice_datamode_cice_restart_read

end module dice_datamode_cice_mod
