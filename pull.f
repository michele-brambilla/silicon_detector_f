*********************************************************************
************** PH e PULL  *******************************************


*************** PH e PULL silici ************************************      
      subroutine calcolapull(isilicio)
      implicit none
      
      include 'variabili.inc'
      real ph_max,pull
      integer isilicio
      
      ph_max=0
      imax(isilicio)=0

      do istrip=1,Nstrip
         
         if (subraw(istrip,isilicio).gt.
     +        cut1(isilicio)*subrms(istrip,isilicio)) then
            call hf1(100000*isilicio,subraw(istrip,isilicio),1.0) ! ph
         end if       
         
         if (subraw(istrip,isilicio).gt.ph_max) then
            ph_max=subraw(istrip,isilicio)
            imax(isilicio)=istrip
         end if
      end do

      if(imax(isilicio).gt.0)then
         pull=ph_max/subrms(imax(isilicio),isilicio)
      else
         pull=-1
      end if

      if (pull.gt.cut1(isilicio))then
         call hf1(100000*isilicio+1,pull,1.0) ! pull
cc         call hf1(1000*isilicio+1,float(imax),1.0) ! posizione del pull

         call pullLR(isilicio)
         call eta_silicio(isilicio)
      end if

      end subroutine calcolapull


*************** PH e PULL basculo ************************************      

      subroutine calcolapull_basculo
      implicit none
      
      include 'variabili.inc'
      real ph_max,pull
      integer isilicio,izone
      

      isilicio=3

      do izone=1,20

         ph_max=0
         imax(isilicio)=0

         do istrip=vec_ROzone(izone,1),vec_ROzone(izone,2)

            if (subraw(istrip,isilicio).gt.cut1(isilicio)*subrms(istrip
     $           ,isilicio)) then
               call hfill(100000*isilicio+10000,float(izone) 
     $              ,subraw(istrip,isilicio),1.0) ! ph-2d
            end if       
            
            if (subraw(istrip,isilicio).gt.ph_max) then
               ph_max=subraw(istrip,isilicio)
               imax(isilicio)=istrip
            end if
            
         enddo !istrip

         if(imax(isilicio).gt.0)then
            pull=ph_max/subrms(imax(isilicio),isilicio)
         else
            pull=-1
         end if

         if (pull.gt.cut1(isilicio))then
            call hfill(100000*isilicio+10001,float(izone),pull,1.0) ! pull-2d
cc          call hf1(1000*isilicio+1,float(imax),1.0) ! posizione del pull

            call pullLR_basculo(izone)
            call eta_basculo(izone)
         end if
         
      enddo  !izone

      end subroutine calcolapull_basculo


      
*********************************************************************
************** pullL e pullR*****************************************


*************** pullLR silici ***************************************      
*     Computes the pull of left and right strip wrt the max ph one
      
      subroutine pullLR(isilicio)
      implicit none
      
      include 'variabili.inc'
      integer isilicio
      real pullL,pullR

      
*     calcolo pull left se imax>1 e plot sopra taglio
      if (imax(isilicio).gt.1)then
         if( subraw(imax(isilicio)-1,isilicio).gt.
     +        cut2(isilicio)*subrms(imax(isilicio)-1,isilicio) )then
            pullL=subraw(imax(isilicio)-1,isilicio)/
     +           subrms(imax(isilicio)-1,isilicio)
            call hf1(100000*isilicio+2,pullL,1.0)
         endif
      end if
*     calcolo pull right se imax<384 e plot sopra taglio
      if (imax(isilicio).lt.384)then
         if( subraw(imax(isilicio)+1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)+1,isilicio) )then
            pullR=subraw(imax(isilicio)+1,isilicio)
     $           /subrms(imax(isilicio)+1,isilicio)
            call hf1(100000*isilicio+3,pullR,1.0)
         end if
      endif

      end subroutine pullLR


*************** pullLR basculo **************************************      
      subroutine pullLR_basculo(izone)
      implicit none
      include 'variabili.inc'
      
      integer,parameter :: isilicio=3
      integer :: izone
      real pullR, pullL
      
*     calcolo pull left se imax>1 e plot sopra taglio
      if (imax(isilicio).gt.1)then
         if( subraw(imax(isilicio)-1,isilicio).gt.
     +        cut2(isilicio)*subrms(imax(isilicio)-1,isilicio) )then
            pullL=subraw(imax(isilicio)-1,isilicio)/
     +           subrms(imax(isilicio)-1,isilicio)
            call hfill(100000*isilicio+10002,float(izone),pullL,1.0) ! pullL-2d
         endif
      end if
*     calcolo pull right se imax<384 e plot sopra taglio
      if (imax(isilicio).lt.384)then
         if( subraw(imax(isilicio)+1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)+1,isilicio) )then
            pullR=subraw(imax(isilicio)+1,isilicio)
     $           /subrms(imax(isilicio)+1,isilicio)
            call hfill(100000*isilicio+10003,float(izone),pullR,1.0) ! pullR-2d
         end if
      endif

      
      end subroutine pullLR_basculo




*********************************************************************
************** ETA **************************************************


*************** ETA silici ******************************************      
*     calcola asimmetria evento
      subroutine eta_silicio(isilicio)
      implicit none
      
      include 'variabili.inc'
      real nb(2),eta,ph_max
      integer isilicio
      
      eta=0
      
      nb(1)=-1
      if( (imax(isilicio).gt.1) ) then
         if (subraw(imax(isilicio)-1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)-1,isilicio)) then
            nb(1)=subraw(imax(isilicio)-1,isilicio)
         end if
      end if
      
      nb(2)=-1
      if( imax(isilicio).lt.Nstrip ) then
         if (subraw(imax(isilicio)+1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)+1,isilicio)) then
            nb(2)=subraw(imax(isilicio)+1,isilicio)
         end if
      end if
      
      ph_max=subraw(imax(isilicio),isilicio)
      
      
cc      print *,'silicio = ', isilicio
cc      print *,'  ','imax(isilicio) = ',imax(isilicio), 'ph = ',ph_max
      if( (nb(1).gt.-1).or.(nb(2).gt.-1) ) then
cc         print *,'  ','L = ',nb(1),'R = ',nb(2)
         if (nb(1).gt.nb(2)) then
            eta=(nb(1)-ph_max)/(nb(1)+ph_max)
cc            print *,'  ','L = ',nb(1),'-> eta = ',eta
         else
            eta=(ph_max-nb(2))/(nb(2)+ph_max)
cc            print *,'  ','R = ',nb(1),'-> eta = ',eta
         end if      
         call hf1(100000*isilicio+4,eta,1.0)   !!eta
cc         print *,''
      end if
cc      print *,''
      
      end subroutine eta_silicio



*************** ETA basculo *****************************************      
      subroutine eta_basculo(izone)
      implicit none
      
      include 'variabili.inc'
      real nb(2),eta,pullL,pullR,ph_max
      integer izone
      integer,parameter :: isilicio=3
      
      eta=0
      
      nb(1)=-1
      if( (imax(isilicio).gt.1) ) then
         if (subraw(imax(isilicio)-1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)-1,isilicio)) then
            nb(1)=subraw(imax(isilicio)-1,isilicio)
         end if
      end if
      
      nb(2)=-1
      if( imax(isilicio).lt.Nstrip ) then
         if (subraw(imax(isilicio)+1,isilicio).gt.cut2(isilicio)
     $        *subrms(imax(isilicio)+1,isilicio)) then
            nb(2)=subraw(imax(isilicio)+1,isilicio)
         end if
      end if
      
      ph_max=subraw(imax(isilicio),isilicio)
      
      
cc      print *,'silicio = ', isilicio
cc      print *,'  ','imax(isilicio) = ',imax(isilicio), 'ph = ',ph_max
      if( (nb(1).gt.-1).or.(nb(2).gt.-1) ) then
cc         print *,'  ','L = ',nb(1),'R = ',nb(2)
         if (nb(1).gt.nb(2)) then
            eta=(nb(1)-ph_max)/(nb(1)+ph_max)
cc            print *,'  ','L = ',nb(1),'-> eta = ',eta
         else
            eta=(ph_max-nb(2))/(nb(2)+ph_max)
cc            print *,'  ','R = ',nb(1),'-> eta = ',eta
         end if
         call hfill(100000*isilicio+10004,float(izone),eta,1.0) ! eta-2d
cc         print *,''
      end if
cc      print *,''
     
      end subroutine eta_basculo
      
