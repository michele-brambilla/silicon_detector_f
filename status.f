*************************************************
*  FUNZIONE PER CREARE IL VETTORE 2D DI ISTATUS *
*************************************************

      subroutine get_status
      implicit none
      
      include 'variabili.inc'


      print *,''
      print *,'subroutine get_status'
      print *,''
      
      open (1,file='status.dat',iostat=istat,status='old')
********* se istat è diverso da 0 -->> significa che non è riuscito a leggere il file
      if(istat.ne.0) then
         close(1)
         
         print *,'Non ho trovato il file status.dat, 
     +lo genero e lo salvo'

*     subroutine impose_cut(isilicio,snr min,snr max,rms min,rms max)

         call impose_cut(1,200.,500.,4.,8.) ! silicio 1x
         
         call impose_cut(2,200.,500.,4.,8.) ! silicio 1y

         call impose_cut(3,300.,600.,3.,6.) ! basculo

         call impose_cut(4,200.,500.,4.,8.) ! silicio 2x

         call impose_cut(5,300.,600.,3.,6.)! silicio 2y

         
         open (1,file='status.dat',iostat=istat,status='new')
         write (1,*) istatus  
         
      else
         
         print *,''
         print *,'Leggo istatus da file'
         print *,''
         
         read (1,*), istatus

      endif
      close(1)

      
      end subroutine get_status





      subroutine impose_cut(isilicio,snrmin,snrmax,rmsmin,rmsmax)

      include 'variabili.inc'

      integer isilicio
      real snr,snrmin,snrmax,rmsmin,rmsmax

      print *,""
      print *,"silicio",isilicio
      print *,""

      do istrip=1,Nstrip
         istatus(istrip,isilicio)=0
         
         snr = subraw(istrip,isilicio)/subrms(istrip,isilicio)
         if( (snr.lt.snrmin).or.(snr.gt.snrmax) )then
            print *,"strip ", istrip, "taglio snr: ",snr
            istatus(istrip,isilicio) = 1    !! => 1 significa strip che non dovrò guardare
         end if
         
         if( (subrms(istrip,isilicio).lt.rmsmin).or.
     +        (subrms(istrip,isilicio).gt.rmsmax) ) then
            print *,"strip ", istrip, "taglio rms: ",subrms(istrip
     $           ,isilicio)
            istatus(istrip,isilicio) = 1
         end if
         
      end do


      end subroutine impose_cut
