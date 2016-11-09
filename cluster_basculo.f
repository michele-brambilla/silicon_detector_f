*****************************
*     cluster
*     convertiamo tutto in cm

      subroutine cluster_basculo(isilicio)

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

*     conteggio il numero di strip nei cluster e le localizzo
      nrScluster=0
      do istrip=1,Nstrip
         if (subraw(istrip,isilicio).gt.
     &        cut1(isilicio)*subrms(istrip,isilicio)) then
            icluster(istrip)=1
            nrScluster=nrScluster+1 ! conteggio strip
         else
            icluster(istrip)=0
         end if

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

      media=nrScluster/nrcluster(isilicio) ! media strip per cluster

      call hf1(1000*isilicio+40,media,1.0)
      call hf1(1000*isilicio+59,float(nrcluster),1.0)

*     caratteristiche del singolo cluster per evento
      nrS = 0
      ampiezza=0
      centro=0
      sigma=0

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

*         print *,"cluster",ic,"baricentro",baricentro_mc(isilicio,ic)
*     $        ,"snr",snr
         
         if(icmax.gt.0)then
            pull=ph_max/subrms(icmax,isilicio)
         end if

         call hf1(100000*isilicio+5,float(nrS),1.0)
         call hf1(100000*isilicio+6,ampiezza,1.0) 
         call hf1(100000*isilicio+7,baricentro_mc(isilicio,ic),1.0)
         call hf1(100000*isilicio+8,snr,1.0)
         call hf1(100000*isilicio+9,pull,1.0)
            
      end do                    ! nrcluster

*      end if                    ! multicluster

 1234 end subroutine cluster_basculo
