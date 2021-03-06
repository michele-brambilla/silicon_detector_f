      program pedestal1

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



c     se sono passati parametri da linea di comando
!     (./pedestal raw/runxxxxxx_000001.hbook runxxxxxx.his)
c     questi determinano il nome del file di input,
c     il nome del file di istogrammi salvato
c     ed il nome del file .dat salvato

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
c     come sempre, questo deve contenere l'id della tupla,
c     il nome del blocco common (vedi common.inc)
c     il primo evento della lista common (o se voi il primo
c     dato salvato nella tupla) ed impostare quello ('$SET')
      call hbname(id,'VFASNTUP',IEVENT,'$SET')
c     recupero il numero di eventi della tupla
      call hnoent(id,nmax)

      print *,'Reading ', nomefile, ': ',nmax,' events'

c     
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

      call hbprof(102,'subprofile-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(103,'subrms-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(202,'subprofile-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(203,'subrms-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(302,'subprofile-Basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(303,'subrms-Basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(402,'subprofile-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(403,'subrms-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(502,'subprofile-Tele2y',384,.5,384.5,0.,4000.,'S')
      call hbprof(503,'subrms-Tele2y',384,.5,384.5,0.,4000.,'S')

      call hbprof(110,'pede-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(210,'pede-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(310,'pede-basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(410,'pede-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(510,'pede-Tele2y',384,.5,384.5,0.,4000.,'S')

      call hbprof(112,'subpede-Tele1x',384,.5,384.5,0.,4000.,'S')
      call hbprof(212,'subpede-Tele1y',384,.5,384.5,0.,4000.,'S')
      call hbprof(312,'subpede-basculo',384,.5,384.5,0.,4000.,'S')
      call hbprof(412,'subpede-Tele2x',384,.5,384.5,0.,4000.,'S')
      call hbprof(512,'subpede-Tele2y',384,.5,384.5,0.,4000.,'S')


      
c     attivo il recupero delle informazioni statistiche sull'histo 100*i
c     +j
      do i=1,5
         do j=0,3
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
      
c     ora common mode e nuovo histo sottratto (102,202,302,402,502)
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
         
         call commonTELE(1)
         call commonTELE(2)
         call commonTELE(4)
         call commonTELE(5)
         call commonBASCULO
      enddo

c     vedi prima
      do i=1,5
         call hunpak(100*i+2,spede(:,i),'',1)
         call hunpke(100*i+2,srms(:,i),'',1)
      enddo

      do i=1,5
         do istrip=1,384
            call hfill(100*i+12,float(istrip),spede(istrip,i),1.0)
            call hfill(100*i+3,float(istrip),srms(istrip,i),1.0)
         enddo
      enddo
      
      call hrend('greta')
      call hrput(0,outfile,'N')
      
c     scrivo anche l'output formattato
      open(unit=10,file=outxt,form='formatted')

      do i=1,5
         do istrip=1,384
            write(10,'(4(F15.5))'), pede(istrip,i), rms(istrip,i),
     $           spede(istrip,i), srms(istrip,i) 
         enddo
      end do
      
      close(10)


      end program pedestal1



************************TELESCOPE1 *********************************
      subroutine commonTELE(isilicio)
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
*         print *,istrip,iasic
         
         gap=iraw_tmp(istrip,isilicio)-pede(istrip,isilicio)
         cm(iasic)=cm(iasic)+gap
      end do
      do iasic=1,3
         cm(iasic)=cm(iasic)/128.0
      end do

*      print *,isilicio
*      print *,cm
*      print *,''

      
      do istrip=1,384
         iasic=(istrip-1)/128+1
         call hfill(100*isilicio+2,float(istrip),iraw_tmp(istrip
     $        ,isilicio)-cm(iasic),1.0)
*         print *,istrip,iasic,iraw_tmp(istrip,isilicio),iraw_tmp(istrip
*     $        ,isilicio)-cm(iasic)
      end do
      
      end subroutine commonTELE






*************************si_400 dump *********************************
      subroutine commonBASCULO
      implicit none

      include 'common.inc'

*     dichiaro un vettor edi tre componenti
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




