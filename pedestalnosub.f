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

      do i=1,5
         if(.not.(i.eq.3)) then
            call status_tele(i)
            print *,''
         else
            call status_basculo(i)
         end if
      end do

      open(unit=10,file='status_test.dat',form='formatted')
      do istrip=1,384
         write(10,*), srms(istrip,:)
      enddo
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



      subroutine status_tele(isili)
      implicit none

      real pede(384,6), spede(384,6)
      real rms(384,6),  srms(384,6)
      real iraw_tmp(384,6)
      common/xpede/pede,rms,spede,srms,iraw_tmp


      real array(384)
      real snr(384)

      real average,sd
      real average1,sd1
      integer,intent(in) :: isili
      integer iasic,first,last,istrip

      array=rms(:,isili)
      snr=pede(:,isili)/rms(:,isili)
      
      do iasic=0,2
         first = 128*iasic+1
         last = 128*(iasic+1)
         
         average = sum(array(first:last))/128.0
         sd = sqrt(sum(array(first:last)**2)/128.0 - average*average)
         average1 = sum(snr(first:last))/128.0
         sd1 = sqrt(sum(snr(first:last)**2)/128.0 - average1*average1)
*         print *,"Average:",average,average1,"Standard Deviation",sd,sd1

         do istrip = first,last
            if( (array(istrip) < average-4*sd).or.(array(istrip) >
     $           average+4*sd).or.
     $           (snr(istrip) < average1-3*sd1).or.(snr(istrip) >
     $           average1+3*sd1) ) then
               print *,isili,',',istrip,
     $              average-4*sd,array(istrip),average+4*sd,',',
     $              average1-3*sd1,snr(istrip),average1+3*sd1
               srms(istrip,isili) = 1
            end if
         end do
      enddo
      
      end subroutine status_tele


      subroutine status_basculo(isili)
      implicit none

      real pede(384,6), spede(384,6)
      real rms(384,6),  srms(384,6)
      real iraw_tmp(384,6)
      common/xpede/pede,rms,spede,srms,iraw_tmp


      real array(384)
      real snr(384)

      real average,sd
      real average1,sd1
      integer,intent(in) :: isili
      integer iasic,first,last,istrip

      array=rms(:,isili)
      snr=pede(:,isili)/rms(:,isili)
      
      do iasic=0,1
         first = 64*iasic+1
         last = 64*(iasic+1)
         average = sum(array(first:last))/64.0
         sd = sqrt(sum(array(first:last)**2)/64.0 - average*average)
         average1 = sum(snr(first:last))/64.0
         sd1 = sqrt(sum(snr(first:last)**2)/64.0 - average1*average1)
         print *, array(first:last)
*     do istrip = first,last
*            if( (array(istrip) < average-4*sd).or.(array(istrip) >
*     $           average+4*sd).or.
*     $           (snr(istrip) < average1-3*sd1).or.(snr(istrip) >
*     $           average1+3*sd1) ) then
*               print *,isili,',',istrip,
*     $              average-4*sd,array(istrip),average+4*sd,',',
*     $              average1-3*sd1,snr(istrip),average1+3*sd1
*               srms(istrip,isili) = 1
*            end if
*         end do
      enddo

      read *,iasic
      iasic = 2
      first = 64*iasic+1
      last = first+59
      print *, array(first:last)
      average = sum(array(first:last))/59.0
      sd = sqrt(sum(array(first:last)**2)/59.0 - average*average)
      average1 = sum(snr(first:last))/59.0
      sd1 = sqrt(sum(snr(first:last)**2)/59.0 - average1*average1)
      do istrip = first,last
         if( (array(istrip) < average-4*sd).or.(array(istrip) >
     $        average+4*sd).or.
     $        (snr(istrip) < average1-3*sd1).or.(snr(istrip) >
     $        average1+3*sd1) ) then
            print *,isili,',',istrip,
     $           average-4*sd,array(istrip),average+4*sd,',',
     $           average1-3*sd1,snr(istrip),average1+3*sd1
            srms(istrip,isili) = 1
         end if
      end do

      read *,iasic
      iasic = 3
      first = 64*iasic+1
      last = 64*(iasic+1)
      print *, array(first:last)
      average = sum(array(first:last))/128.0
      sd = sqrt(sum(array(first:last)**2)/64.0 - average*average)
      average1 = sum(snr(first:last))/128.0
      sd1 = sqrt(sum(snr(first:last)**2)/64.0 - average1*average1)
      do istrip = first,last
         if( (array(istrip) < average-4*sd).or.(array(istrip) >
     $        average+4*sd).or.
     $        (snr(istrip) < average1-3*sd1).or.(snr(istrip) >
     $        average1+3*sd1) ) then
            print *,isili,',',istrip,
     $           average-4*sd,array(istrip),average+4*sd,',',
     $           average1-3*sd1,snr(istrip),average1+3*sd1
            srms(istrip,isili) = 1
         end if
      end do

      iasic = 4
      first = 64*iasic+1
      last = first+16
      average = sum(array(first:last))/16.0
      sd = sqrt(sum(array(first:last)**2)/16.0 - average*average)
      average1 = sum(snr(first:last))/16.0
      sd1 = sqrt(sum(snr(first:last)**2)/16.0 - average1*average1)
*     print *,"Average:",average,average1,"Standard Deviation",sd,sd1
      do istrip = first,last
         if( (array(istrip) < average-4*sd).or.(array(istrip) >
     $        average+4*sd).or.
     $        (snr(istrip) < average1-3*sd1).or.(snr(istrip) >
     $        average1+3*sd1) ) then
            print *,isili,',',istrip,
     $           average-4*sd,array(istrip),average+4*sd,',',
     $           average1-3*sd1,snr(istrip),average1+3*sd1
            srms(istrip,isili) = 1
         end if
      end do


      
      end subroutine status_basculo
