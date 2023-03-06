module datm_datamode_copyall_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use shr_const_mod    , only : shr_const_TkFrz, shr_const_pi, shr_const_ocn_ref_sal
  use shr_sys_mod      , only : shr_sys_abort
  use dshr_methods_mod , only : dshr_state_getfldptr, dshr_fldbun_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_mod         , only : dshr_restart_read, dshr_restart_write
  use dshr_strdata_mod , only : shr_strdata_type

  implicit none
  private ! except

  public  :: datm_datamode_copyall_advertise
  public  :: datm_datamode_copyall_init_pointers
  public  :: datm_datamode_copyall_advance
  public  :: datm_datamode_copyall_restart_read
  public  :: datm_datamode_copyall_restart_write

  ! export fields
  real(r8), pointer :: Sa_pslv(:) => null() ! sea level pressure
  real(r8), pointer :: Sa_u10m(:) => null() ! zonal wind height 10m
  real(r8), pointer :: Sa_v10m(:) => null() ! meridional wind height 10m

  character(*) , parameter :: nullstr = 'null'
  character(*) , parameter :: rpfile  = 'rpointer.atm'
  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_datamode_copyall_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sa_pslv')
    call dshr_fldList_add(fldsExport, 'Sa_u10m')
    call dshr_fldList_add(fldsExport, 'Sa_v10m')

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(datm_comp_advertise): Fr_atm'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine datm_datamode_copyall_advertise

  !===============================================================================
  subroutine datm_datamode_copyall_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! initialize pointers to export fields
    call dshr_state_getfldptr(exportState, 'Sa_pslv', fldptr1=Sa_pslv, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_u10m', fldptr1=Sa_u10m, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sa_v10m', fldptr1=Sa_v10m, allowNullReturn=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (associated(Sa_pslv)) then
      Sa_pslv(:) = 0.0_r8
    end if
    if (associated(Sa_u10m)) then
      Sa_u10m(:) = 0.0_r8
    end if
    if (associated(Sa_v10m)) then
      Sa_v10m(:) = 0.0_r8
    end if

  end subroutine datm_datamode_copyall_init_pointers

  !===============================================================================
  subroutine datm_datamode_copyall_advance(mainproc, logunit, mpicom, rc)

    ! input/output variables
    logical                , intent(in)    :: mainproc
    integer                , intent(in)    :: logunit
    integer                , intent(in)    :: mpicom
    integer                , intent(out)   :: rc

    ! local variables
    integer  :: n                   ! indices
    integer  :: lsize               ! size of attr vect
    character(len=*), parameter :: subname='(datm_datamode_copyall_advance): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !lsize = size(Sa_pslv)

    !do n = 1,lsize
    !   !--- bottom layer height ---
    !   if (.not. associated(strm_z)) Sa_z(n) = 30.0_r8
    !end do

  end subroutine datm_datamode_copyall_advance

  !===============================================================================
  subroutine datm_datamode_copyall_restart_write(case_name, inst_suffix, ymd, tod, &
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

    call dshr_restart_write(rpfile, case_name, 'datm', inst_suffix, ymd, tod, &
         logunit, my_task, sdat)

  end subroutine datm_datamode_copyall_restart_write

  !===============================================================================
  subroutine datm_datamode_copyall_restart_read(rest_filem, inst_suffix, logunit, my_task, mpicom, sdat)

    ! input/output arguments
    character(len=*)            , intent(inout) :: rest_filem
    character(len=*)            , intent(in)    :: inst_suffix
    integer                     , intent(in)    :: logunit
    integer                     , intent(in)    :: my_task
    integer                     , intent(in)    :: mpicom
    type(shr_strdata_type)      , intent(inout) :: sdat
    !-------------------------------------------------------------------------------

    call dshr_restart_read(rest_filem, rpfile, inst_suffix, nullstr, logunit, my_task, mpicom, sdat)

  end subroutine datm_datamode_copyall_restart_read

end module datm_datamode_copyall_mod
