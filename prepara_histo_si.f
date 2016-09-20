********************************
*     prepara gli histo

      subroutine prepara_histo_si()
      implicit none

      include 'variabili.inc'   
      
      integer isilicio
      do isilicio=1,Nsilicio

*     pull e PH (per silicio e per strip)
         call hbook1(isilicio*100000,'PH',1000,0.5,5000.5,0.)
         call hbook1(isilicio*100000+1,'Pull',1000,0.5,1000.5,0.)

*     pullL, pullR e ETA (per silicio e per strip)
         call hbook1(isilicio*100000+2,'PullL',1000,0.5,1000.5,0.)
         call hbook1(isilicio*100000+3,'PullR',1000,0.5,1000.5,0.)
*         call hbook1(isilicio*100000+4,'eta',200,-2.,2.,0.)
         call hbook1(isilicio*100000+4,'eta',50,-1.,1.,0.)

*     cluster
         call hbook1(isilicio*100000+5,'nr Strip',384,0.5,384.5,0.)
         call hbook1(isilicio*100000+6,'ampiezza',1000,0.5,5000.5,0.) 
         call hbook1(isilicio*100000+7,'baricentro cluster',100,0.5,10.5
     $        ,0.)
         call hbook1(isilicio*100000+8,'SNR',1000,0.5,3000.5,0.)
         call hbook1(isilicio*100000+9,'Pull',1000,0.5,1500.5,0.)
      
      enddo

*     PH e pull 2D per basculo per le 20 zone
      call hbook2(310000,'PH-2d',20,0.5,20.5,1500,0.5,1500.5,0.)
      call hbook2(310001,'Pull-2d',20,0.5,20.5,1500,0.5,1500.5,0.)
      
*     pullL, pullR e ETA per basculo per le 20 zone
      call hbook2(310002,'PullL-2d',20,0.5,20.5,1500,0.5,1500.5,0.)
      call hbook2(310003,'PullR-2d',20,0.5,20.5,1500,0.5,1500.5,0.)
*      call hbook2(310004,'eta-2d',20,0.5,20.5,200,-2.,2.,0.)
      call hbook2(310004,'eta-2d',20,0.5,20.5,50,-1.,1.,0.)
            


      
      end subroutine prepara_histo_si
