*****************************
*     cluster
*     convertiamo tutto in cm

      subroutine cluster(isilicio)


      include 'variabili.inc'

      character*50 chline

      integer isilicio,ic
      integer derivata(Nstrip)
      integer icluster(Nstrip)
      integer nrS,icmax
      
      real media,centro,noise,sigma,pull
      real nrScluster
      real baricentro
      real sigmacluster
      real snr
      real ampiezza

      real display(Nstrip)

      ! decommentare
*      call hreset(1000*isilicio+68,'event-display')
*      call hreset(1000*isilicio+69,'cut')

*     conteggio il numero di strip nei cluster e le localizzo
      nrScluster=0
*      call find_cluster(isilicio,icluster,nrScluster)
      do istrip=1,Nstrip
         if (subraw(istrip,isilicio).gt.
     &        cut1(isilicio)*subrms(istrip,isilicio)) then
            icluster(istrip)=1
            nrScluster=nrScluster+1 ! conteggio strip
         else
            icluster(istrip)=0
         end if

*! decommentare
**     event display
*         call hf1(1000*isilicio+68,float(istrip)*pitch,subraw(istrip
*     $        ,isilicio))
*         call hf1(1000*isilicio+69,float(istrip)*pitch,cut1(isilicio)
*     $        *subrms(istrip,isilicio))

      end do

      
*     conta il numero di cluster nell'evento nella strip (creando la
*     derivata) delle strip che registrano segnale
      if(icluster(1).gt.0)then
         derivata(1)=1
         nrcluster(isilicio)=1
      else
         derivata(1)=0
         nrcluster(isilicio)=0
      end if
      do istrip=2,Nstrip
         derivata(istrip)=icluster(istrip)-icluster(istrip-1)
         if( derivata(istrip).gt.0 )then
            nrcluster(isilicio)=nrcluster(isilicio)+1
         end if
      end do

*      if(nrcluster(isilicio).gt.1) then
*         call HPLSET('HCOL',1)
*         call HPLOT(1000*isilicio+68,' ',' ',' ')
*         call HPLSET('HCOL',2)
*         call hplot(1000*isilicio+69,'S',' ',' ') ! taglio
*         call IGTERM
*         do istrip=1,Nstrip
*            print *,subraw(istrip,isilicio),cut1(isilicio)*subrms(istrip
*     $           ,isilicio),icluster(istrip),derivata(istrip)
*         end do
*         call kuinps('enter>',chline,len)
*      end if
      
      media=nrScluster/nrcluster(isilicio) ! media strip per cluster

      call hf1(1000*isilicio+40,media,1.0)
      call hf1(1000*isilicio+59,float(nrcluster),1.0)

*     caratteristiche del singolo cluster per evento
      nrS = 0
      ampiezza=0
      centro=0
      sigma=0

*      if( nrcluster(isilicio).eq.1 ) then ! se ho 1 solo cluster e'
*                                          ! semplice ottenere le
*                                          ! informazioni
*
*         ipmax=0
*         
*         do istrip=1,Nstrip
*            if( icluster(istrip).eq.1 ) then
*               nrS=nrS+1
*               ampiezza=ampiezza+
*     &              subraw(istrip,isilicio)
*               centro=centro+(subraw(istrip,isilicio)*istrip)
*               sigma=sigma+(subrms(istrip,isilicio))**2
*
*               if( (subraw(istrip,isilicio)/subrms(istrip
*     $              ,isilicio)).gt.ipmax ) then
*                  icmax=istrip
*                  ipmax=subraw(istrip,isilicio)/subrms(istrip,isilicio)
*               end if
*
*            end if
*         end do
*
*         baricentro_mc(isilicio,1)=centro/ampiezza
*         pull_mc(isilicio,1)=icmax*pitch
*         ampiezza_mc(isilicio,1)=ampiezza
*         
*         sigmacluster=sigma/nrS
*         noise=sqrt(sigmacluster)
*         snr=ampiezza/(noise*
*     +        sqrt(real(nrS)))
*
*         call hf1(1000*isilicio+50,float(nrS),1.0)
*         call hf1(1000*isilicio+52,ampiezza,1.0) 
*         call hf1(1000*isilicio+55,baricentro_mc(isilicio,1),1.0)
*         call hf1(1000*isilicio+56,snr,1.0)
*
*         baricentro_mc(isilicio,1)=baricentro_mc(isilicio,1)
*     $        *pitch            ! converto la posizione del baricentro in cm
*
*      else ! se ho più di 1!cluster

      istrip=1
      
      do ic=1,nrcluster(isilicio)
         do while(icluster(istrip).eq.0)

            istrip = istrip+1
            if (istrip.gt.Nstrip) then
               goto 1234
            endif
         enddo
         ampiezza=0.
         nrS = 0
         centro=0
         sigma=0
         ph_max=0.
         icmax=0
         do while(icluster(istrip).eq.1)
            call hf1(1000*isilicio+68,subraw(istrip,isilicio),1.0)

            ampiezza=ampiezza+
     &           subraw(istrip,isilicio)
            nrS=nrS+1
            centro=centro+(subraw(istrip,isilicio)*x_conv(istrip
     $           ,isilicio))
            sigma=sigma+(subrms(istrip,isilicio))**2
            if (subraw(istrip,isilicio).gt.ph_max) then
               ph_max=subraw(istrip,isilicio)
               icmax=istrip
            end if   

            istrip = istrip+1
            if (istrip.gt.Nstrip) then
               goto 1234
            end if

         end do

         baricentro_mc(isilicio,ic)=centro/ampiezza
         ampiezza_mc(isilicio,ic)=ampiezza            
         sigmacluster=sigma/nrS
         noise=sqrt(sigmacluster)
         snr=ampiezza/(noise*
     +        sqrt(real(nrS)))

         if(icmax.gt.0)then
            pull=ph_max/subrms(icmax,isilicio)
         end if
         
*         baricentro_mc(isilicio,ic)=baricentro_mc(isilicio,ic)
*     $        *pitch(1,ic)            ! converto la posizione del baricentro in cm

         call hf1(100000*isilicio+5,float(nrS),1.0)
         call hf1(100000*isilicio+6,ampiezza,1.0) 
         call hf1(100000*isilicio+7,baricentro_mc(isilicio,ic),1.0)
         call hf1(100000*isilicio+8,snr,1.0)
         call hf1(100000*isilicio+9,pull,1.0)
            
      end do                    ! nrcluster

*      end if                    ! multicluster

 1234 end

      
*****************************
*     correlazione
**     
*      subroutine correlazione
*
*      include 'variabili.inc'
**      include 'common.inc'
*      
*      real x(Nsilicio/2), z(Nsilicio/2), sigma(Nsilicio/2)
*      real m,q,sigmam,sigmaq,chi2
*
*
*      z(1)=100.
*      z(2)=143.3
*      z(3)=171.6
*
*!!! x(4-i) per il problema delle camere invertite      
*      do i=1,Nsilicio/2         !fit per n-silicio lungo x
*         x(4-i)=baricentro_mc(1+2*(i-1),1)
*         sigma(i)=sigmax        ! da align (histo 7001,7003)
*      enddo
*      
*      call fitlineare(z,x,sigma,m,q,sigmam,sigmaq,chi2)
*
*      betax=atan(m)
*      call  hf1(8001,betax,1.0)
*      
*      do i=1,Nsilicio/2         !fit per n-silicio lungo y
*         x(4-i)=baricentro_mc(2*i,1)
*         sigma(i)=sigmay        ! da align (histo 7002,7004)
*      enddo
*      
*      call fitlineare(z,x,sigma,m,q,sigmam,sigmaq,chi2)
*      betay=atan(m)
*      call  hf1(8002,betay,1.0) 
*      call hfill(8000,betax,betay,1.)
*
*      end subroutine correlazione
*
**************************************************************************
*!     applica "correlazione" solo alla direzione "direzione" e mette il
*!     corrispondente dato nell'istogramma idhisto. Questo permette di
*!     usare questa subroutine per trovare le informazione nel caso in cui
*!     una delle due direzioni abbia un solo cluster e l'altra piu' di
*!     uno.
*!     Esempio di chiamata:
*!     call correlazione_direzione(8001,'x')
*
*      subroutine correlazione_direzione(idhisto, direzione)
*
*      include 'variabili.inc'
*      integer idhisto
*      character direzione
*      
*      real x(Nsilicio/2), z(Nsilicio/2), sigma(Nsilicio/2)
*      real m,q,sigmam,sigmaq,chi2
*      real beta
*
*      z(1)=100.
*      z(2)=143.3
*      z(3)=171.6
*
*      if( direzione.eq.'x' ) then
*         
*         do i=1,Nsilicio/2      !fit per n-silicio lungo x
*            x(i)=baricentro_mc(1+2*(i-1),1)
*            sigma(i)=sigmax     ! da align (histo 7001,7003)
*         enddo
*      
*         call fitlineare(z,x,sigma,m,q,sigmam,sigmaq,chi2)
*
*         betax=atan(m)
*         betay = 0
*         beta = betax
*
*      else
*         
*         do i=1,Nsilicio/2      !fit per n-silicio lungo y
*            x(i)=baricentro_mc(2*i,1)
*            sigma(i)=sigmay     ! da align (histo 7002,7004)
*         enddo
*         
*         call fitlineare(z,x,sigma,m,q,sigmam,sigmaq,chi2)
*
*         betax = 0
*         betay=atan(m)
*         beta = betay
*         
*      endif
*      
*      call  hf1(idhisto,beta,1.0)
*      
*      end subroutine correlazione_direzione
**********************************************************************
*      
*
*
**     algoritmo originale per determinare i cluster. tutte le strip che
**     superano il valore di soglia partecipano al cluster
*      subroutine find_cluster(isilicio,icluster,nrScluster)
*
*      include 'variabili.inc'
*
*      integer isilicio
*      integer icluster(Nstrip)
*      real nrScluster
*      
*      do istrip=1,Nstrip         
*         if (subraw(istrip,isilicio).gt.
*     &        cut1(isilicio)*subrms(istrip,isilicio)) then
*            icluster(istrip)=1
*            nrScluster=nrScluster+1 ! conteggio strip
*         else
*            icluster(istrip)=0
*         end if
*      end do
*
*      end subroutine find_cluster
*
*
*
**     algoritmo originale per determinare i cluster. solo le strip
**     contigue (+-2) che superano il valore di soglia partecipano al
**     cluster
*      subroutine find_cluster_nb(isilicio,icluster,nrScluster)
*
*      include 'variabili.inc'
*
*      integer isilicio
*      integer icluster(Nstrip)
*      real nrScluster
*      
*      do istrip=1,Nstrip         
*         if (subraw(istrip,isilicio).gt.
*     &        cut1(isilicio)*subrms(istrip,isilicio)) then
*            icluster(istrip)=1
*            nrScluster=nrScluster+1 ! conteggio strip
*         else
*            icluster(istrip)=0
*         end if
*      end do
*
*      end subroutine find_cluster_nb
*
*      
