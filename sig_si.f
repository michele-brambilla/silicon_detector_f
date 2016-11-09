      function sig(iev)
      implicit none

      include 'variabili.inc'
      include 'common.inc'

      integer,parameter :: is = 4

      real sig
      integer iev,i,start,isilicio
      real x_baric,y_baric,m_x(10),m_y(10)

      character chline(1)


!     se SONO eventi LED
      if ( INFO_PLUS(2).lt.0 )then 
         
         goto 33333
         
      else  !! da qui eseguo se NON sono eventi LED
      
***************************camere [Si] ********************************
         
         do istrip=1,384
            raw(istrip,1)=IVFAS_DATA1(is+istrip)    !!telescope 1_x
            raw(istrip,2)=IVFAS_DATA2(is+istrip)    !!telescope 1_y
            raw(istrip,4)=IVFAS_DATA5(is+istrip)    !!telescope 2_x
            raw(istrip,5)=IVFAS_DATA6(is+istrip)    !!telescope 2_y
         enddo

         do istrip=1,272
            raw(istrip,3)=IVFAS_DATA3(is+istrip)    !!basculo
         enddo
         
         call calcolasubraw_sil_telescope !camere [Si]
         call calcolasubraw_sil_basculo   !camere [Si]

         
*******!!! cerco CLUSTER sui Silici 

         do isilicio=1,Nsilicio
            if (isilicio.eq.3) then
               call calcolapull_basculo !!sub in pull.f
            endif
            
            call calcolapull(isilicio) !!sub in pull.f

            if (isilicio.eq.3) then
               call cluster_basculo(isilicio) !!calc baricentro_mc => sub in cluster.f
            else
               call cluster(isilicio)
            endif
            
         end do
         


      endif                     !chiudo 'if' eventi LED


      lastevent=IEVENT
      
33333 sig = 1.0
            
      
      end function sig











      
******************** SUBROUTINE calcolasubraw ********************
*** camere [Si] telescope1 e telescope2 ***
      subroutine calcolasubraw_sil_telescope

      include 'variabili.inc'
      integer cont
      real CM(nasic),gap,cutgap

      cutgap=5  !considero i contributi al common mode solo se il (segnale-pede) > cutgap*subrms
      
      do isilicio=1,Nsilicio
         if (.NOT.(isilicio.eq.3)) then
            do iasic=1,nasic
               sum=0
               cont=0
               
               do i=1,nchan
                  istrip=i+(iasic-1)*nchan
                  if(istatus(istrip,isilicio).eq.0) then
                     gap=raw(istrip,isilicio)-subpede(istrip,isilicio)
                     if (gap.gt.cutgap*subrms(istrip,isilicio))then
                        cont=cont+1
                        sum=sum+gap
                     endif
                  endif
               enddo
               if(cont.gt.0)then
                  CM(iasic)=sum/float(cont)
               else
                  CM(iasic)=0
               end if

               do i=1,nchan  !nchan = 128
                  istrip=i+(iasic-1)*nchan
                  if(istatus(istrip,isilicio).eq.0) then
                     subraw(istrip,isilicio)=raw(istrip,isilicio)
     &                    -subpede(istrip,isilicio)-CM(iasic)
                  else 
                     subraw(istrip,isilicio)=0.
                     subrms(istrip,isilicio)=0
                  endif
               end do

            end do              ! asic

*            print *,cm
            
         endif
      end do                    ! silicio
      
      end subroutine calcolasubraw_sil_telescope



*** [Si] basculo ***
      subroutine calcolasubraw_sil_basculo

      include 'variabili.inc'
      integer cont
      real CM(nasic_b),gap,cutgap

      cutgap=5  !considero i contributi al common mode solo se il (segnale-pede) > cutgap*subrms
      
      isilicio=3

      do iasic=1,nasic_b
         sum=0
         cont=0

         do i=1,nchan_b
            istrip=i+(iasic-1)*nchan_b
            if(istatus(istrip,isilicio).eq.0) then
               gap=raw(istrip,isilicio)-subpede(istrip,isilicio)
               if (gap.gt.cutgap*subrms(istrip,isilicio))then
                  cont=cont+1
                  sum=sum+gap
               endif
            endif
         enddo
         if(cont.gt.0)then
            CM(iasic)=sum/float(cont)
         else
            CM(iasic)=0
         end if

         do i=1,nchan_b           !nchan_b = 64
            istrip=i+(iasic-1)*nchan_b
            if(istatus(istrip,isilicio).eq.0) then
               subraw(istrip,isilicio)=raw(istrip,isilicio)
     &              -subpede(istrip,isilicio)-CM(iasic)
            else 
               subraw(istrip,isilicio)=0.
               subrms(istrip,isilicio)=0
            endif
         end do

      end do                    ! asic
            
      
      end subroutine calcolasubraw_sil_basculo
      
      
