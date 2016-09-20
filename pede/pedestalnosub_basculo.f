      program pedestalnosub_basculo

      include 'common.inc'

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
      do isilicio=1,5
         call hbprof(100*isilicio+0,'pede',384,.5,384.5,0.,4000.,'S')
         call hbprof(100*isilicio+1,'rms', 384,.5,384.5,0.,4000.,'S')
      end do
      
c     attivo il recupero delle informazioni statistiche sull'histo 100*i
c     +j
      do i=1,5
         call hidopt(100*i+0,'STAT');
         call hidopt(100*i+1,'STAT');
      enddo
      
c     riempie il profilo     
      do iev = 1,nmax
         call hgnt(1,iev,ierr)

         do istrip=1,384
            iraw_tmp(istrip,1)=IVFAS_DATA1(4+istrip)
            iraw_tmp(istrip,2)=IVFAS_DATA2(4+istrip)
            iraw_tmp(istrip,3)=IVFAS_DATA3(4+istrip)
            iraw_tmp(istrip,4)=IVFAS_DATA5(4+istrip)
            iraw_tmp(istrip,5)=IVFAS_DATA6(4+istrip)
         enddo

         call profilo
      enddo

      do i=1,5
         call hunpak(100*i,pede(:,i),'',1)
         call hunpke(100*i,rms(:,i),'',1)
      enddo

      do i=1,5
         do istrip=1,384
            call hfill(100*i+1,float(istrip),rms(istrip,i),1.0)
         enddo
      enddo
      
      call hrend('greta')
      call hrput(0,outfile,'N')
      
c     scrivo anche l'output formattato
      open(unit=10,file=outxt,form='formatted')

      do i=1,5
         do istrip=1,384
            write(10,'(2(F15.5))'), pede(istrip,i), rms(istrip,i)
         enddo
      end do
      
      close(10)

      end program pedestalnosub_basculo





      
****************************************
      subroutine profilo
      include 'common.inc'
*     implicit none

      integer istrip
      real pede(384,6),spede(384,6)
      real rms(384,6),srms(384,6)
      real iraw_tmp(384,6)
      common/xpede/pede,rms,spede,srms,iraw_tmp
      
      do isilicio=1,2
         do istrip=1,384
            call hfill(100*isilicio,float(istrip),iraw_tmp(istrip
     $           ,isilicio),1.0)
         end do
      end do

      isilicio=3
      do istrip=1,272
         if( (istrip.lt.131).or.(istrip.gt.135) ) then
            call hfill(100*isilicio,float(istrip),iraw_tmp(istrip
     $           ,isilicio),1.0)
         end if
      end do

      do isilicio=4,5
         do istrip=1,384
            call hfill(100*isilicio,float(istrip),iraw_tmp(istrip
     $           ,isilicio),1.0)
         end do
      end do

      end subroutine profilo




