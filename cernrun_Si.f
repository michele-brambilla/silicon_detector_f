      program cernrun_Si
      
      common/pawc/h(99999999)

      include 'common.inc'
      include 'variabili.inc'

      integer icycle,i,jnew

!     nome del file di output
      character*50 output
      character*50 ctmp
      
      integer nfilemax,firstrun,lastrun
      real tmp
!     INIZIALIZZO

      open(unit=5,file='cut.txt',iostat=istat,status='old')
      do isili=1,5
         read(5,*),cut1(isili),cut2(isili)
      end do
      close(5)
      do isili=1,Nsilicio
         print *,isili,cut1(isili),cut2(isili)
      end do
      
      call ini_pitch

      
!     tiene traccia dell'evento nei LED       
      lastevent=0
      
      call hlimit(99999999)
      
!     legge in "input" le informazioni per il run
      call parse(nfilemax,firstrun,lastrun,output,output_dir,do_display)

      
********************************************************      
********************************************************

!     prepara hi
!     ntuple più grosse --> non 1024, ma 4096
      call hropen(2,'analisi',trim(output_dir)//"/"//trim(output),'N'
     $     ,4096,istat)
      if(istat.ne.0) then
         print *,''
         print *,'Errore: impossibile scrivere il file ',
     $        trim(output_dir)//"/"//trim(output)
         stop
      endif

      call prepara_histo_si
      call hldir('//','')

      call basculo_ROzone
      
      if(do_display.gt.0) then
         call hplint(1)
      endif

      

************ j è l'id del run --> INPUT **************
 3333 do j=firstrun,lastrun               
******************************************************

!     legge pede, ricostruisce nome pede  dal "file".dat
         call leggi_pede(j,inew)

         
************ i è _000? del run --> INPUT *************         
         do i=1,nfilemax
******************************************************

!     carica i dati         
            call apri_tupla(i,j)
            call hldir('//','')
            if(istat.ne.0) goto 999

            do iev=1,nmax
               call hgnt(1,iev,ierr)
               if(ierr.ne.0) goto 999

cc               if(IVFAS_DATA3(5).gt.0) then !!controllo che la I info
               call sig(iev)    !SUB in sig_si.f
cc               endif
            enddo
            
            call hdelet(1)
            call hrend('dati')
            close (40)
            
         enddo

 999     print *,'Fine run'
         print *
      enddo
************ chiudo ciclo j (id pede/run)*************************

      call hplend(1)
      
      call hldir('//','')

      call hcdir('//analisi','')
      call hrout(0,icycle,'')
      call hrend('analisi')
      close(2)




      
!     stampo a terminale informazioni
 4096 print *
      print *
      print *,"Fine analisi"
      print *

      end program cernrun_Si








**************************************
**************************************
*inizializzo silici a passo varibile (cm)

      subroutine ini_pitch
      implicit none 
      include 'variabili.inc'

      integer isilicio
      real fstrip

      
      do isilicio=1,Nsilicio,3
         pitch(1,isilicio)=0.0025 !cm
         x_conv(1,isilicio)=0.00125 ! cm
         do istrip=2,Nstrip
            pitch(istrip,isilicio)=0.0025 !cm
            x_conv(istrip,isilicio)=x_conv(istrip-1,isilicio)+0.005 ! cm
         enddo
      enddo

      do isilicio=2,Nsilicio,3
         pitch(1,isilicio)=0.005 !cm
         x_conv(1,isilicio)=0.0025 ! cm
         do istrip=2,Nstrip
            pitch(istrip,isilicio)=0.005 !cm
            x_conv(istrip,isilicio)=x_conv(istrip-1,isilicio)+0.005 ! cm
         enddo
      enddo
*      do istrip=1,384
*         print *,istrip,x_conv(istrip,1:2),x_conv(istrip,4:5)
*      end do
      
      isilicio=3
      open(unit=5,file='silidampe.dat',iostat=istat,status='old')
      do istrip=1,272
         read(5,*),fstrip,x_conv(istrip,isilicio)
         if( .not.(floor(fstrip).eq.istrip) ) then
            print *,'Errore in ini_pitch: strip
     +         letta ed attesa non coincidono'
         end if
      end do
      close(5)
      do istrip=1,272
         print *,istrip,x_conv(istrip,isilicio)
      end do
      end subroutine ini_pitch
      


********************************
*     legge pede e srms


      subroutine leggi_pede(j,inew)
      implicit none 

      include 'variabili.inc'

      integer j,inew
      character(len=1024) :: filename
      real pede,rms
***************************************************************************
**** I4.4= 4caratteri su cui gira e.4 dice che se è <4 riempie a sx con 0

      write(filename, "(A7,I6.6,A9)")
     +     "rax/run",j,"_pede.dat"

      print *,filename
      open (unit=5,file=trim(filename)
     +     ,iostat=istat,status='old')
********* se istat è =/ da 0 -->> significa che nn riesce a leggere il file
      if(istat.ne.0) then
         close(5)
         print *,'Non ho trovato il file', trim(filename)
         inew=1
         stop
      else

         do isili=1,Nsilicio
            do istrip=1,Nstrip
               read(5,*),pede,rms,subpede(istrip,isili),subrms(istrip
     $              ,isili)

cc               read(5,*), subpede(istrip,isili), subrms(istrip,isili),
cc     +              subrms(istrip,isili)

            enddo
         enddo

         close(5)

         call get_status  !!SUB in status.f

         inew=0
         return

      endif
      end
      



********************************
*     apre le tuple
      
      subroutine apri_tupla(fid,j)
      implicit none 

      include 'common.inc'
      include 'variabili.inc'

      character(len=1024) :: filename
C     
      integer j


      write(filename, "(A7,I6.6,A1,I5.5,A6)")
     +     "raw/run",j,"_",fid,".hbook"

      print *,filename

      call hropen(40,'dati',trim(adjustl(filename))
     +     ,'',4096,istat)
      
      if(istat.ne.0) return
      print *,trim(adjustl(filename))
      call hrin(1,99999,0)
      call hbname(1,' ',0,'$clear')
      call hbname(1,'VFASNTUP',ievent,'$set')
      call hnoent(1,nmax)
      print *,'n events = ', nmax
      
      end subroutine apri_tupla


      
*****************************************
*     Funzioni di appoggio per leggere il file di input
      

      subroutine parse(nfile,first,last,output,output_directory
     $     ,do_displayevent)
      implicit none
      
      character*50 opt
      character*50 name
      character*50 output
      character*50 output_directory
      integer istat,first,last,nfile,do_displayevent
      
      open (unit=2,file='input',iostat=istat,status='old')
      if(.not.istat.eq.0) then
         close(1)
         print *,'Non ho trovato il file di input'
         istat = -1
         stop
      end if

      read(2,*), opt,nfile
      name='nfilemax'
      call check_parse(name,opt,1,nfile,istat)
      read(2,*), opt,first
      name='first'
      call check_parse(name,opt,91,first,istat)
      read(2,*), opt,last
      name='last'
      call check_parse(name,opt,96,last,istat)


      read(2,*), opt,output
      name='output'
      call check_parse_text(name,opt,output,istat)
      read(2,*), opt,output_directory
      name='output_directory'
      call check_parse_text(name,opt,output_directory,istat)


      read(2,*), opt,do_displayevent
      name='display_event'
      call check_parse(name,opt,0,do_displayevent,istat)

      end subroutine parse

      

      subroutine check_parse(what,name,minvalue,value,istat)
      implicit none
      
      character*50 what
      character*50 name
      integer value,minvalue,istat
      
      if( (.not.trim(what).eq.trim(name)).or.(value.lt.minvalue) ) then
         print *,'Errore: ',trim(name)
     $        ,' non trovato o valore inferiore a ',minvalue
         istat = -1
         stop
      else
         istat =  0
      endif

      end subroutine check_parse


      subroutine check_parse_real(what,name,minvalue,value,istat)
      implicit none
      
      character*50 what
      character*50 name
      real value,minvalue
      integer istat
      
      if( (.not.trim(what).eq.trim(name)).or.(value.lt.minvalue) ) then
         print *,'Errore: ',trim(name)
     $        ,' non trovato o valore inferiore a ',minvalue
         istat = -1
         stop
      else
         istat =  0
      endif

      end subroutine check_parse_real


      
      subroutine check_parse_text(what,name,value,istat)
      implicit none
      
      character*50 what
      character*50 name
      character*50 value
      integer istat

      if( (.not.trim(what).eq.trim(name)).or.(len(trim(value)).lt.1) )
     $     then
         print *,'Errore: ',trim(name),' non trovato'
         istat = -1
         stop
      else
         istat =  0
      endif

      end subroutine check_parse_text
