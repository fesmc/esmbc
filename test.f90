program test

    use precision
    use ncio 

    use ismip6
    use varslice 

    implicit none 

    type(ismip6_experiment_class) :: iexp 
    type(ismip6_forcing_class) :: ismp 
    type(varslice_class)       :: v1 

    real(wp) :: time_init, time_end, time, dt
    integer  :: n, k

    
if (.FALSE.) then
    ! Testing methods on one varslice variable ==============

    call make_test_file("var_test.nc",t0=1950.0_wp,dt=1.0_wp/12.0_wp,nt=11*12)
    
    
    call varslice_init_nml(v1,"par/varslice.nml",group="var1")
    
    call varslice_update(v1, [1945.15_wp],method="interp",rep=1, print_summary=.TRUE.)
    call varslice_update(v1, [1959.15_wp],method="interp",rep=1, print_summary=.TRUE.)
    call varslice_update(v1, [1959.0_wp], method="interp",rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1945.15_wp],method="extrap",rep=1, print_summary=.TRUE.)
    call varslice_update(v1, [1959.0_wp], method="extrap",rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1965.15_wp],method="extrap",rep=1, print_summary=.TRUE.)
    call varslice_update(v1, [1945.0_wp], method="extrap",rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1965.0_wp], method="extrap",rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1954.0_wp,1956.0_wp],method="range",     rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1954.0_wp,1956.0_wp],method="range_mean",rep=12,print_summary=.TRUE.)
    call varslice_update(v1, [1950.0_wp,1960.0_wp],method="range_mean",rep=12,print_summary=.TRUE.)

    call varslice_update(v1, [1959.0_wp], method="exact",rep=12,print_summary=.TRUE.)
    
    stop 
end if


if (.TRUE.) then
    ! Testing methods on one varslice variable in multiple yearly files ==============

    call make_test_files("var_test_*.nc",t0=1950.0_wp,dt=1.0_wp/12.0_wp,nt=11*12)
    
    call varslice_init_nml(v1,"par/varslice.nml",group="var1")
    
end if


    ! =======================================================


if (.FALSE.) then

    ! === Testing output writing ===

    ! call ismip6_write_step(filename="icesheet_ismip6.nc",file_nml="ismip6.nml",time=0.0_wp)
    ! stop 

    ! ======================================================================

    ! Running Antarctica domain, load Antarctica specific parameters
    call ismip6_experiment_def(iexp,"ctrlAE","par/ismip6_ant.nml","UCM","YELMO")

    ! Initialize variables inside of ismip6 object 
    call ismip6_forcing_init(ismp,"par/ismip6_ant.nml","Antarctica","ANT-32KM", &
                                experiment=iexp%experiment,shlf_collapse=iexp%shlf_collapse)
    
    ! Print some information for static variables
    write(*,*) "================" 
    call print_var_range(ismp%ts_ref%var, "ts_ref", mv) 
    call print_var_range(ismp%pr_ref%var, "pr_ref", mv) 
    call print_var_range(ismp%smb_ref%var,"smb_ref",mv) 
    write(*,*) "----"
    call print_var_range(ismp%to_ref%var, "to_ref", mv) 
    call print_var_range(ismp%so_ref%var, "so_ref", mv) 
    call print_var_range(ismp%tf_ref%var, "tf_ref", mv) 
    call print_var_range(ismp%tf_cor%var, "tf_cor", mv) 
    write(*,*) 

    ! ======================================================================
    
    ! Perform timestepping
    time_init = 1840.0 
    time_end  = 2110.0 
    dt        = 10.0 

    do n = 1, ceiling((time_end-time_init)/dt)+1

        ! Get current time 
        time = time_init + (n-1)*dt

        ! Update ismip6 forcing to current time
        call ismip6_forcing_update(ismp,time)

        ! Check data
        write(*,*) "================"
        call print_var_range(ismp%ts%var, "ts", mv,time) 
        call print_var_range(ismp%pr%var, "pr", mv,time) 
        call print_var_range(ismp%smb%var,"smb",mv,time) 
        write(*,*) "----"
        call print_var_range(ismp%to%var, "to", mv,time) 
        call print_var_range(ismp%so%var, "so", mv,time) 
        call print_var_range(ismp%tf%var, "tf", mv,time) 
        write(*,*) 

        ! Write an output file
        call write_step("test.nc",ismp%tf%var(:,:,1,1),time,init= (time .eq. time_init) )

    end do

    write(*,*) "Done testing ismip6 forcing."
    write(*,*)

end if

contains

    subroutine make_test_file(filename,t0,dt,nt)

        implicit none 

        character(len=*),   intent(IN) :: filename 
        real(wp),           intent(IN) :: t0
        real(wp),           intent(IN) :: dt
        integer,            intent(IN) :: nt
        
        ! Local variables
        integer :: k  
        real(wp), allocatable :: var(:,:,:) 

        ! Create the netcdf file 
        call nc_create(filename)

        ! Add grid axis variables to netcdf file
        call nc_write_dim(filename,"xc",x=[1,2,3],units="1")

        call nc_write_dim(filename,"yc",x=[2,4,6],units="1")
        
        ! Add time axis with current value 
        call nc_write_dim(filename,"time", x=t0+dt/2.0_wp,dx=dt,nx=nt,units="years",unlimited=.TRUE.)
        
        allocate(var(3,3,nt))

        do k = 1, nt 
            var(:,:,k) = t0 + dt/2.0_wp + real(k-1,wp)*dt
        end do 

        call nc_write(filename,"var1",var,dim1="xc",dim2="yc",dim3="time")

        return 

    end subroutine make_test_file

    subroutine make_test_files(filename,t0,dt,nt)

        implicit none 

        character(len=*),   intent(IN) :: filename 
        real(wp),           intent(IN) :: t0
        real(wp),           intent(IN) :: dt
        integer,            intent(IN) :: nt
        
        ! Local variables
        character(len=1024) :: filename_now
        integer :: i, n_files
        character(len=4) :: yr_str
        real(wp) :: t 

        n_files = int(nt*dt)

        do i = 1, n_files
            t = t0+(i-1)
            write(yr_str,"(i4)") int(t)
            call replace_substring(filename_now, filename, "*", yr_str)
            call make_test_file(filename_now,t,dt,nt=int(1.0/dt))
        end do

        return

    end subroutine make_test_files

    subroutine replace_substring(output_str, input_str, old_sub, new_sub)
        
        implicit none
        
        character(len=*), intent(OUT) :: output_str  ! Modified string
        character(len=*), intent(IN)  :: input_str   ! Original string
        character(len=*), intent(IN)  :: old_sub     ! Substring to replace
        character(len=*), intent(IN)  :: new_sub     ! Substring to replace with
        
        ! Local variables
        character(len=255) :: temp_str
        integer :: pos, old_len, new_len, input_len

        ! Initialize output_str to input_str initially
        output_str = input_str
        old_len = len(old_sub)
        new_len = len(new_sub)
        input_len = len(input_str)

        ! Loop to replace all occurrences of old_sub with new_sub
        do
            pos = index(output_str, old_sub)
            if (pos == 0) exit  ! No more occurrences found

            ! Perform replacement
            temp_str = output_str(1:pos-1) // new_sub // output_str(pos+old_len:)

            ! Update output_str with the modified string
            output_str = temp_str
        end do

        return

    end subroutine replace_substring

    subroutine write_step(filename,var,time,init)

        implicit none 

        character(len=*), intent(IN) :: filename 
        real(wp),         intent(IN) :: var(:,:) 
        real(wp),         intent(IN) :: time
        logical,          intent(IN) :: init 

        ! Local variables
        integer  :: nx, ny, n, ncid

        nx = size(var,1) 
        ny = size(var,2) 

        if (init) then 
            ! Create the netcdf file 
            call nc_create(filename)

            ! Add grid axis variables to netcdf file
            call nc_write_dim(filename,"xc",x=1,dx=1,nx=nx,units="1")
            call nc_write_dim(filename,"yc",x=1,dx=1,nx=ny,units="1")
        
            ! Add time axis with current value 
            call nc_write_dim(filename,"time", x=time,dx=1.0_wp,nx=1,units="years",unlimited=.TRUE.)
        
        end if  

        call nc_open(filename,ncid=ncid,writable=.TRUE.)

        ! Determine current writing time step 
        n = nc_time_index(filename,"time",time,ncid=ncid)

        ! Update the time step
        call nc_write(filename,"time",time,dim1="time",start=[n],count=[1],ncid=ncid)

        ! Write the variable
        call nc_write(filename,"var",var,dim1="xc",dim2="yc",dim3="time", &
                                                start=[1,1,n],count=[nx,ny,1],ncid=ncid)

        ! Close the netcdf file
        call nc_close(ncid)

        return 

    end subroutine write_step

    
end program test



