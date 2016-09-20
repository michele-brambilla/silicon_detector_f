      program pedestalsub

      include 'common.inc'
      include '../variabili.inc'
      
      parameter (NWPAWC=2000000)
      common/pawc/H(NWPAWC)

      real pede(384,6), spede(384,6)
      real rms(384,6),  srms(384,6)
      real iraw_tmp(384,6)
      common/xpede/pede,rms,spede,srms,iraw_tmp

      character*100 in
      character*100 nomefile
      character*100 outfile
      character*100 outxt

      integer is
      
      call getarg(1, in)
      if(len_trim(in).ne.0) then
         nomefile = in
      else
         nomefile='runxxx_001.hbook'
      endif
      call getarg(2, in)
      if(len_trim(in).ne.0) then
         outfile = in
      else
         outfile='runxxx.his'
      endif

      ll=len_trim(outfile)
      outxt=outfile
      outxt(ll-2:ll)='dat'

c     fine parsing della linea di comando. stampo info
      print *,'Input ', nomefile
      print *,'Histo output ',outfile
      print *,'Text output  ',outxt


c     leggo l'output formattato di pede, rms
      open(unit=10,file=outxt,form='formatted')
      do i=1,5
         do istrip=1,384
            read(10,'(2(F15.5))'), pede(istrip,i), rms(istrip,i)
         end do
      end do
      close(10)

      inquire(FILE='status.dat', EXIST=file_exists)
      call get_status
      
      call hlimit(NWPAWC)

c     ora inizia il programma. recupero le tuple
      id=1
      call hropen(id,'greta',nomefile,' ',4096,istat)
      if (istat.ne.0) then
         print *,' Pbs in opening file ',nomefile
         return
      endif
c     apro ed inizio a leggere      
      call hrin(id,99999,0)
      call hbname(id,' ',0,'$CLEAR')
      call hbname(id,'VFASNTUP',IEVENT,'$SET')
      call hnoent(id,nmax)

      print *,'Reading ', nomefile, ': ',nmax,' events'

      call hstaf('YES')

c     creo gli istogrammi
      do i=1,5
         call hbprof(100*i+2,'sprofile', 384,.5,384.5,0.,4000.,'S')
         call hbprof(100*i+3,'srms',     384,.5,384.5,0.,4000.,'S')
      end do
      
c     attivo il recupero delle informazioni statistiche sull'histo 100*i
c     +j
      do i=1,5
         call hidopt(100*i+2,'STAT');
         call hidopt(100*i+3,'STAT');
      enddo
      
      
c     ora common mode e nuovo histo sottratto (102,202,302,402,502)
      is=4
      do iev = 1,nmax
         call hgnt(1,iev,ierr)
         
         do istrip=1,384
            iraw_tmp(istrip,1)=IVFAS_DATA1(is+istrip)
            iraw_tmp(istrip,2)=IVFAS_DATA2(is+istrip)
            iraw_tmp(istrip,3)=IVFAS_DATA3(is+istrip)
            iraw_tmp(istrip,4)=IVFAS_DATA5(is+istrip)
            iraw_tmp(istrip,5)=IVFAS_DATA6(is+istrip)
         enddo
         
         do isilicio=1,2
            call common(isilicio)
         end do

         do istrip=131,135
            iraw_tmp(istrip,3)=0
         end do
         call commonBASCULO1
         
         do isilicio=4,5
            call common(isilicio)
         end do

      enddo
      
c     vedi prima
      do isilicio=1,5
         call hunpak(100*isilicio+2,spede(:,isilicio),'',1)
         call hunpke(100*isilicio+2,srms(:,isilicio),'',1)
      enddo

      do isilicio=1,5
         do istrip=1,384
            call hfill(100*isilicio+3,float(istrip),srms(istrip
     $           ,isilicio),1.0)
         enddo
      enddo
      
      call hrend('greta')
      call hrput(0,outfile,'N')
      
c     scrivo anche l'output formattato
      open(unit=10,file=outxt,form='formatted')
      
      do isilicio=1,5
         do istrip=1,384
            write(10,'(4(F15.5))'), pede(istrip,isilicio), rms(istrip
     $           ,isilicio),spede(istrip,isilicio), srms(istrip
     $           ,isilicio) 
         end do

      end do
      
      close(10)


      end program pedestalsub



************************TELESCOPE *********************************
      subroutine common(isilicio)
c      implicit none
      
      include 'common.inc'
      include '../variabili.inc'

*     dichiaro un vettor edi tre componenti
      real cm(3)
      real iraw_tmp(384,6),gap
      real pede(384,6), spede(384,6)
      real rms(384,6), srms(384,6)
      integer i,j,iasic,isilicio,count(3)
      common/xpede/pede,rms,spede,srms,iraw_tmp

      
      do iasic=1,3
         cm(iasic)=0.0
         count(iasic)=0
      end do
      
      do istrip=1,384
         
         if(istatus(istrip,isilicio).eq.0) then
            iasic=(istrip-1)/128+1
            
            gap=iraw_tmp(istrip,isilicio)-pede(istrip,isilicio)
            cm(iasic)=cm(iasic)+gap
            count(iasic)=count(iasic)+1
         end if                 ! istatus
      end do
      
      do iasic=1,3
         cm(iasic)=cm(iasic)/count(iasic)
      end do
         
      do istrip=1,384
         
         if(istatus(istrip,isilicio).eq.0) then
            iasic=(istrip-1)/128+1
            call hfill(100*isilicio+2,float(istrip),iraw_tmp(istrip
     $           ,isilicio)-cm(iasic),1.0)
         else
            call hfill(100*isilicio+2,float(istrip),0.,1.0)
         end if                 ! istatus
      end do
         
      end subroutine common









************************* si_400 dump *********************************
      subroutine commonBASCULO
c      implicit none

      include 'common.inc'
      include '../variabili.inc'

*dichiaro un vettor edi tre componenti
      real cmBasculo(5)
      real iraw_tmp(384,6)
      real pede(384,6), spede(384,6)
      real rms(384,6),  srms(384,6)
      integer i,j,iasic
      common/xpede/pede,rms,spede,srms,iraw_tmp

      do iasic=1,5

         cmBasculo(iasic)=iraw_tmp(1+(iasic-1)*64,3)-pede(1+(iasic-1)
     $        *64,3)            !!!punto di partenza dell'asic
         
         if (iasic.lt.5) then
            if (.NOT.(iasic.eq.3)) then
               
               do j=2,64
                  cmBasculo(iasic)=cmBasculo(iasic)+iraw_tmp((iasic-1)
     $                 *64+j,3)-pede((iasic-1)*64+j,3)
               enddo
               
            else
               cmBasculo(iasic)=iraw_tmp(2+(iasic-1)*64,3)-pede(2+(iasic
     $              -1)*64,3)

               do j=8,64
                  cmBasculo(iasic)=cmBasculo(iasic)+iraw_tmp((iasic-1)
     $                 *64+j,3)-pede((iasic-1)*64+j,3)
               enddo
            end if

         else
            do j=2,16
               cmBasculo(iasic)=cmBasculo(iasic)+iraw_tmp((iasic-1)*64+j
     $              ,3)-pede((iasic-1)*64+j,3)
            enddo
         endif
      
         if (iasic.lt.5) then
            if (.NOT.(iasic.eq.3)) then
               cmBasculo(iasic)=cmBasculo(iasic)/64.0 !i primi 3 asic hanno 68 strip/cad.
            else
               cmBasculo(iasic)=cmBasculo(iasic)/59.0 !il IV asic ha 59 (buco da 5 strip) strip
            endif
         else
            cmBasculo(iasic)=cmBasculo(iasic)/16.0 !il V asic ha solo 16 strip
         endif

      enddo


      do iasic=1,5

         if (iasic.lt.5) then
            if (.NOT.(iasic.eq.3)) then
               
               do i=1,64
                  call hfill(302,float(i+(iasic-1)*64),iraw_tmp(i+(iasic
     $                 -1)*64,3)-cmBasculo(iasic),1.0)
               enddo
            else
               do i=1,2
                  call hfill(302,float(i+(iasic-1)*64),iraw_tmp(i+(iasic
     $                 -1)*64,3)-cmBasculo(iasic),1.0)
               enddo
               do i=8,64
                  call hfill(302,float(i+(iasic-1)*64),iraw_tmp(i+(iasic
     $                 -1)*64,3)-cmBasculo(iasic),1.0)
               enddo
            endif

         else
            do i=1,16
               call hfill(302,float(i+(iasic-1)*64),iraw_tmp(i+(iasic-1)
     $              *64,3)-cmBasculo(iasic),1.0)
            enddo
         end if
         
      enddo


      end   




************************* si_400 dump *********************************
      subroutine commonBASCULO1
c      implicit none

      include 'common.inc'
      include '../variabili.inc'

*dichiaro un vettor edi tre componenti
      real cmBasculo(5)
      real iraw_tmp(384,6)
      real pede(384,6), spede(384,6)
      real rms(384,6),  srms(384,6)
      integer iasic,count(5)

      common/xpede/pede,rms,spede,srms,iraw_tmp

      do iasic=1,5
         cmBasculo(iasic)=0.0
         count(iasic) = 0
      end do
      
      do istrip=1,272
         if( istatus(istrip,3).eq.0) then
            iasic=(istrip-1)/64+1
            
            if( (istrip.lt.131).or.(istrip.gt.135) ) then
               cmBasculo(iasic)=cmBasculo(iasic)+iraw_tmp(istrip,3)
     $              -pede(istrip,3)
               count(iasic)=count(iasic)+1
            end if

         end if                 ! istatus
      end do
      
      do iasic=1,5
         cmBasculo(iasic) = cmBasculo(iasic)/float(count(iasic))
      end do
      
      do istrip=1,272
         if( istatus(istrip,3).eq.0) then
            
            iasic=(istrip-1)/64+1
            
            if( (istrip.lt.131).or.(istrip.gt.135) ) then
               call hfill(302,float(istrip),iraw_tmp(istrip
     $           ,3)-cmBasculo(iasic),1.0)
            end if

         else
            call hfill(302,float(istrip),0.0,1.0)
         end if                 ! istatus
      end do
         
      
      end subroutine commonBASCULO1
