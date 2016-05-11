
      program main
      implicit none

        character(len=120) :: line, arg
        character(len=120) :: raw_input ,  form_input
        integer   ::  iostatus, counter, i, block_size
        integer   ::  nblock, iblock, icount
        double precision ::  dvdl, avg, avg2
        integer    :: idat, oform

        integer    ntot
        integer    converg_flag
        double precision   var_all, convr_threshold, ptc_old

        common /dp/ var_all, convr_threshold, ptc_old
        common /integ/ converg_flag
        common /char/  raw_input, form_input


        convr_threshold=5e-2
        block_size=0
        converg_flag=0
        ptc_old=-100.0
        counter=0
        icount=1
        dvdl=0.0
        avg=0.0
        avg2=0.0
        idat=100
        oform=101

        call getarg(1,arg)
        !write(*,*) "len of arg,", len(trim(arg)) 
        if (len(trim(arg)) .eq. 0 ) then
            write(*,*) "Usage:"
            write(*,*) "  ./a.out file.dat [Threshold [block_size]] "
            write(*,*) "  Block_size is an integer and optional, if not"
            write(*,*) "  specified, will loop to converged block_size."
            write(*,*) "  Threshold is set for block average &
                            convergence."
            call exit()
        end if
        raw_input=trim(arg)
        form_input="form_"//trim(arg)


        call getarg(2,arg)
        if (len(trim(arg)) .gt. 0 ) then
            read(arg,*) convr_threshold
            if (convr_threshold .gt. 9.0) then
                block_size=int(convr_threshold)
            else
                write(*,'("Threshold",e15.8 , " is used.")') convr_threshold
            end if
        else
            write(*,'("Default threshold",e15.8 , " is used.")') convr_threshold
        end if

        !call getarg(3,arg)
        !!write(*,*) "len of arg,", len(trim(arg)) 
        !if (len(trim(arg)) .gt. 0 ) then
        !    read(arg,*) block_size
        !end if
    







     ! find global average&variation and format data
       open(idat,file=raw_input)
       open(oform, file=form_input)
      do while (.true.) 
          read(idat,'(a)',IOSTAT=iostatus) line
          !write(*,*) line
          if ( iostatus < 0 ) then
              exit  ! end of file
          end if

            i=index(line,'average')
            if (i.gt.10) then
                cycle
            else
                counter=counter+1
                read(line,'(t8,f15.8)') dvdl
                write(oform,'(f15.8)') dvdl
                avg=avg+dvdl
                avg2=avg2+dvdl*dvdl
            end if
        end do
        close(oform)
        ntot=counter
        avg=avg/ntot
        avg2=avg2/ntot
        var_all=avg2-avg*avg
        write(*,*) "Total number of data: ", ntot


        ! find converged block size
        if (block_size .eq. 0) then
            do while (block_size .lt. ntot) 
                if (converg_flag .eq. 1) then
                    write(*,*) "Converged, Block size is ", block_size
                    exit
                end if
                block_size=block_size+10
                call block_avg(ntot,block_size)
            end do
        else
            call block_avg(ntot,block_size)
        end if 


        end program main






        subroutine block_avg(ntot,block_size)
        implicit none

        integer  ::   ntot,  block_size

        character(len=120) ::  line
        character(len=120) :: raw_input ,  form_input
        integer   ::  iostatus, i, iblock, nblock, icount,nrest
        double precision  ::   avg, avg2,dvdl, var_mean

        integer  ::  obavg,ibavg, iform
        integer   ::  converg_flag
        double precision   var_all, ptc_old, ptc_new,convr_threshold
        double precision   :: per_error
        common /dp/ var_all, convr_threshold, ptc_old
        common /integ/ converg_flag
        common /char/ raw_input, form_input

        !block_size = read from parameter
        nblock=ntot/block_size
        write(*,*) "block_size, ", block_size
        nrest=mod(ntot,block_size)
        if (nrest .gt. 0) then
            write(*,*) "The last ", nrest, "data are droped."
        end if


        iblock=1
        icount=1
        avg=0.0
        avg2=0.0
        dvdl=0.0
        obavg=100
        iform=102
        ibavg=103


        open(obavg,file="block_average_"//raw_input,status="replace")
        open(iform,file=form_input)

        do while (.true.)
          read(iform,'(f15.8)',IOSTAT=iostatus) dvdl
          if ( iostatus < 0 ) then
              !write(*,*) "Exit by end of file"
              exit  ! end of file
          else
              avg=avg+dvdl
              avg2=avg2+dvdl*dvdl

              if (icount .lt. block_size) then
                  icount =icount+1
              else
                  avg=avg/icount
                  avg2=avg2/icount
                  
                  write(obavg,'(I5,f15.8,f15.8)') iblock, avg, avg2-avg*avg
                  avg=0.0
                  avg2=0.0
                  icount=1
                  if (iblock .eq. nblock) then
                      !write(*,*) "Exit by bilock"
                      exit
                  end if
                  iblock=iblock+1
              end if
          end if

        end do
        close(obavg)   
        close(iform)


        if (nblock .ne. 1) then  ! if nblock = 1, no average is needed.

        ! find var of mean at certain block_size
        open(unit=ibavg,file="block_average_"//raw_input) 
        avg=0.0
        avg2=0.0
        do while (.true.)
          read(ibavg,'(t6,f15.8)',IOSTAT=iostatus) dvdl
          if ( iostatus < 0 ) then
              exit  ! end of file
          else
            avg=avg+dvdl
            avg2=avg2+dvdl*dvdl
          end if
        end do
        avg=avg/nblock
        avg2=avg2/nblock

            var_mean=avg2-avg*avg
            ptc_new=block_size*var_mean/var_all
            write(1000,*) "ptc_new,", ptc_new
            if (ptc_old .ne. -100.0 ) then
                per_error=(ptc_new-ptc_old)/ptc_old
                write(*,*) "per_error, " , per_error
                if (abs(per_error) .lt. convr_threshold) then
                    converg_flag=1
                end if
                ptc_old=ptc_new
            else
                ptc_old=ptc_new
            end if
            write(*,*)  " ptc_old, ",ptc_old

        !write(*,*) "The last ", nrest, "data are droped."
        close(ibavg)   
        open(unit=obavg,file="block_average_"//raw_input,access="append")
            write(obavg,*) "Average of Mean: " , avg
            write(obavg,*) "Standard Error of Mean:" , sqrt(var_mean/nblock)
        close(obavg)

        end if 
       end subroutine
