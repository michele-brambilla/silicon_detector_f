      program pedestalsub

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
      do i=1,6
         do istrip=1,384
            read(10,'(2(F15.5))'), pede(istrip,i), rms(istrip,i)
         enddo
      end do
      close(10)

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
      do i=1,6
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
            iraw_tmp(istrip,4)=IVFAS_DATA4(is+istrip)
            iraw_tmp(istrip,5)=IVFAS_DATA5(is+istrip)
            iraw_tmp(istrip,6)=IVFAS_DATA6(is+istrip)
         enddo
         
         do isilicio=1,6
            call common(isilicio)
         end do
         
      enddo
      
c     vedi prima
      do isilicio=1,6
         call hunpak(100*isilicio+2,spede(:,isilicio),'',1)
         call hunpke(100*isilicio+2,srms(:,isilicio),'',1)
      enddo

      do isilicio=1,6
         do istrip=1,384
            call hfill(100*isilicio+3,float(istrip),srms(istrip
     $           ,isilicio),1.0)
         enddo
      enddo
      
      call hrend('greta')
      call hrput(0,outfile,'N')
      
c     scrivo anche l'output formattato
      open(unit=10,file=outxt,form='formatted')
      
      do isilicio=1,6
         do istrip=1,384
            write(10,'(4(F15.5))'), pede(istrip,isilicio), rms(istrip
     $           ,isilicio),spede(istrip,isilicio), srms(istrip
     $           ,isilicio) 
         enddo
      end do
      
      close(10)


      end program pedestalsub



************************TELESCOPE1 *********************************
      subroutine common(isilicio)
      implicit none
      
      include 'common.inc'

*     dichiaro un vettor edi tre componenti
      real cm(3)
      real iraw_tmp(384,6),gap
      real pede(384,6), spede(384,6)
      real rms(384,6), srms(384,6)
      integer i,j,iasic,istrip,isilicio
      common/xpede/pede,rms,spede,srms,iraw_tmp

      
      do iasic=1,3
         cm(iasic)=0.0
      end do
      do istrip=1,384
         iasic=(istrip-1)/128+1
         
         gap=iraw_tmp(istrip,isilicio)-pede(istrip,isilicio)
         cm(iasic)=cm(iasic)+gap
      end do

      do iasic=1,3
         cm(iasic)=cm(iasic)/128.0
      end do

      do istrip=1,384
         iasic=(istrip-1)/128+1
         call hfill(100*isilicio+2,float(istrip),iraw_tmp(istrip
     $        ,isilicio)-cm(iasic),1.0)
      end do
      
      end subroutine common


