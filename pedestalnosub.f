      program pedestalnosub

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
      is=4
      
      call hlimit(NWPAWC)


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
      call hbprof(100,'profile-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(101,'rms-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(200,'profile-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(201,'rms-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(300,'profile-Basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(301,'rms-Basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(400,'profile-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(401,'rms-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(500,'profile-Tele2y',384,.5,384.5,0.,4000.,'S')
      call hbprof(501,'rms-Tele2y',384,.5,384.5,0.,4000.,'S')

      call hbprof(110,'pede-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(210,'pede-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(310,'pede-basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(410,'pede-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(510,'pede-Tele2y',384,.5,384.5,0.,4000.,'S')
      
c     attivo il recupero delle informazioni statistiche sull'histo 100*i
c     +j
      do i=1,5
         do j=0,1
            call hidopt(100*i+j,'STAT');
         enddo
      enddo
      
c     riempie il profilo     
      do iev = 1,nmax
         call hgnt(1,iev,ierr)

         do istrip=1,384
            iraw_tmp(istrip,1)=IVFAS_DATA1(is+istrip)
            iraw_tmp(istrip,2)=IVFAS_DATA2(is+istrip)
            iraw_tmp(istrip,4)=IVFAS_DATA5(is+istrip)
            iraw_tmp(istrip,5)=IVFAS_DATA6(is+istrip)
         enddo

         do istrip=1,272
            iraw_tmp(istrip,3)=IVFAS_DATA3(is+istrip)
         enddo

         call profilo
      enddo

      do i=1,5
         call hunpak(100*i,pede(:,i),'',1)
         call hunpke(100*i,rms(:,i),'',1)
      enddo

      do i=1,5
         do istrip=1,384
            call hfill(100*i+10,float(istrip),pede(istrip,i),1.0)
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

      end program pedestalnosub





      
****************************************
      subroutine profilo
      include 'common.inc'
*     implicit none

      integer istrip
      real pede(384,6),spede(384,6)
      real rms(384,6),srms(384,6)
      real iraw_tmp(384,6)
      common/xpede/pede,rms,spede,srms,iraw_tmp
      
      do istrip=1,384
         call hfill(100,float(istrip),iraw_tmp(istrip,1),1.0)
         call hfill(200,float(istrip),iraw_tmp(istrip,2),1.0)
         call hfill(300,float(istrip),iraw_tmp(istrip,3),1.0)
         call hfill(400,float(istrip),iraw_tmp(istrip,4),1.0)
         call hfill(500,float(istrip),iraw_tmp(istrip,5),1.0)
      enddo

c     scrivo anche l'output formattato
      open(unit=11,file='basculo',form='formatted')

      do istrip=1,384
         write(11,'(I5,(F15.5))'), IVFAS_DATA3(4+istrip),
     $        iraw_tmp(istrip,3)
      enddo
      
      close(11)



      end subroutine profilo




