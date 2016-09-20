********************************
*     prepara gli histo

      subroutine prepara_histo()

      include 'variabili.inc'

      do isilicio=1,Nsilicio
         
*     pull e PH (per silicio e per strip)
         call hbook1(isilicio*1000+5,'pull-taglio',1000,0.5,500.5,0.)
         call hbook1(isilicio*1000+6,'PH-taglio',1000,0.5,1500.5,0.)
         call hbook1(isilicio*1000+1,'pull-posizione',384,0.5,384.5,0.)

*     cluster
         call hbook1(isilicio*1000+55,'baricentro',384,.5,384.5,0.)

         call hbook1(isilicio*1000+68,'segnale',384,.5,384.5,0.)
         call hbook1(isilicio*1000+69,'taglio',384,.5,384.5,0.)

      enddo

*     correlazione
      call hbook2(6000,'correl-x',384,.5,384.5,384,.5,384.5,0.)
      call hbook2(7000,'correl-y',384,.5,384.5,384,.5,384.5,0.)
      call hbook2(8000,'angular-distrib',100,-3.1416,3.1416,100,
     +     -3.1416,3.1416,0.)
      
      call hbook1(8001,'angular-distrib-x',100,-0.1,0.1,0.) ! 1 cluster
      call hbook1(8002,'angular-distrib-y',100,-0.1,0.1,0.)

      call hbook1(8011,'ang-distr-x',100,-0.1,0.1,0.) ! 1 cluster
      call hbook1(8511,'ang-distr-x',100,-0.1,0.1,0.) ! 1 cluster
      call hbook1(8611,'ang-distr-x',100,-0.1,0.1,0.) ! 1 cluster
      call hbook1(8012,'ang-distr-y',100,-0.1,0.1,0.)
      call hbook1(8512,'ang-distr-y',100,-0.1,0.1,0.)
      call hbook1(8612,'ang-distr-y',100,-0.1,0.1,0.)
      


************* maskmaroc *********
      call hbook1(9000,'phTOTcalo-energia',500,0.5,12000.5,0.)
      call hbook1(9500,'phTOTcrist5-energia',500,0.5,12000.5,0.)

************* equalization *********
      do i=1,36
         call hbook2(1900+i,'2d equalization',60,0.,300000.,1000,1000.
     $        ,3000.,0.)
         call hbook2(1940+i,'2d post equalization',60,0.,300000.,1000
     $        ,1000.,3000.,0.)
         call hbprof(1800+i,'equalization',60,0.,300000.,1000.,3000.,'')
         call hbprof(1700+i,'post equalization',60,0.,300000.,1000.
     $        ,3000.,'')

      end do
      
************* linearity *********
      
      call hbook2(9001,'dcalo1sui6si-x',1000,.5,12000.5,100,-5.5,10.5
     $     ,0.)
      call hbook2(9002,'dcalo1sui6si-y',1000,.5,12000.5,100,-5.5,10.5
     $     ,0.)

      call hbook2(9501,'dcalo1cri5-x',1000,.5,12000.5,100,-5.5,10.5,0.)
      call hbook2(9502,'dcalo1cri5-y',1000,.5,12000.5,100,-5.5,10.5,0.)
      
      call hbook2(9011,'dcalo-x',1000,.5,12000.5,100,.5,10.5,0.)
      call hbook2(9012,'dcalo-y',1000,.5,12000.5,100,.5,10.5,0.)

      call hbook2(9511,'dcalo-cri5-x',1000,.5,12000.5,100,.5,10.5,0.)
      call hbook2(9512,'dcalo-cri5-y',1000,.5,12000.5,100,.5,10.5,0.)

*taglio
      call hbook1(9100,'phTOTtaglioMIP-energia',500,0.5,12000.5,0.)
      call hbook1(9200,'phTOTtaglioPOS-energia',500,0.5,12000.5,0.)
      call hbook1(9300,'plot energia pulito',500,0.5,12000.5,0.)

*ph_sipm (barre)
      call hbook1(9400,'ph_sipm_naive_all (barre)',500,0.5,12000.5,0.)
      do i=1,Nbarre
         call hbook1(9400+i,'ph_sipm_naive (barre)',500,0.5,12000.5,0.)
         call hbook1(9410+i,'ph_sipm (barre)',500,0.5,12000.5,0.)
      enddo
      
******** pull **********************************************

      do isipm=1,Ncanali
         call hbook1(10000+isipm,'pull-taglio',1000,0.5,1500.5,0.)
      enddo
      call hbook1(10037,'pull-cumulativo',1000,0.5,1500.5,0.)
      call hbook1(10038,'pull-posizione',36,0.5,36.5,0.)
      call hbook2(10039,'griglia',6,0.,10.,6,0.,10.,0.)

      call hbook2(10040,'pull_calo_2d_display',6,0.,10.,6,0.,10.,0.) ! hi/2d x display-event sul calorimetro
      call hbook2(10140,'pull_calo_2d_display',6,0.,10.,6,0.,10.,0.) ! hi/2d x display-event sul calorimetro

*     call hbook2(10041,'pull_calo_proiez',6,0.,10.,6,0.,10.,0.) ! hi
*   /2d

** effic      
      call hbook2(10041,'pull_calo_proiez',50,0.,10.,50,0.,10.,0.) ! hi/2d
      call hbook2(10141,'pull_calo_proiez',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset
      
      call hbook2(11041,'se-calo_proiez',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset

***   eff 2 cluster
      call hbook2(20141,'pull_calo_proiez',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset
      call hbook2(21041,'se-calo_proiez',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset

***   eff 2 cluster RANGE
      call hbook2(22141,'pull_calo_RANGE',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset
      call hbook2(22041,'se-calo_RANGE',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset

******event-display 2 tracce
c      call hbook2(30141,'pull_calo_proiez',50,-8.,12.,50,-8.,12.,0.) ! hi/2d no reset
      call hbook2(30141,'adc-proiez-Si',50,-3.,12.,50,-3.,12.,0.) ! hi/2d no reset

      call hbook2(39410,'adc-cristalli',5,-2.,3.,5,-1.,4.,0.)! hi/2d no reset

******hi-range 
      call hbook2(30142,'hit-2tracce',50,-3.,12.,50,-3.,12.,0.)!2 coppie cristalli diverse
      call hbook2(30143,'hit-2tracce',50,-3.,12.,50,-3.,12.,0.)!1 coppia cristalli
      call hbook2(30144,'hit-2tracce',50,-3.,12.,50,-3.,12.,0.)!stesso cristallo

      call hbook1(30145,'ph-tracce',300,1.,3000.,0.)
      call hbook1(30146,'ph-no-tracce',300,1.,3000.,0.)

      call hbook1(30147,'ph-tracce-relativo',100,0.,1.,0.)
      call hbook1(30148,'ph-no-tracce-relativo',100,0.,1.,0.)

      call hbook1(30149,'ph-1traccia',300,1.,3000.,0.)
      call hbook1(30150,'ph-no-1traccia',300,1.,3000.,0.)

      call hbook1(30151,'ph-1traccia-relativo',100,0.,1.,0.)
      call hbook1(30152,'ph-no-1traccia-relativo',100,0.,1.,0.)

      call hbook1(30153,'spettro-tracce',100,1.,3000.,0.)
      call hbook1(30154,'spettro-no-tracce',100,1.,3000.,0.)
      call hbook1(30155,'spettro-1traccia',100,1.,3000.,0.)
      call hbook1(30156,'spettro-no-1traccia',100,1.,3000.,0.)

****  hi-range/calorimetro
*      call hbook1(40000,'nrcluster=2cristalli',10,0.,10.,0.)

      call hbook1(40000,'2cluster=n-cristalli',12,-1.5,10.5,0.)
      call hbook1(40001,'2cluster=n-cristalli',12,-1.5,10.5,0.)
      call hbook1(40002,'2cluster=n-cristalli',12,-1.5,10.5,0.)
      call hbook1(40003,'2cluster=n-cristalli',12,-1.5,10.5,0.)
      call hbook1(40004,'2cluster=n-cristalli',12,-1.5,10.5,0.)
      




      
      
***eta_calo
      call hbook1(11141,'eta-x',200,-2.,2.,0.) !eta_x del calo nell'evento x_c
      call hbook1(111411,'eta-y',200,-2.,2.,0.)!eta_y del calo nell'evento x_c
      do i=1,Nbarre
         call hbook1(19410+i,'ph_sipm (barre)',500,0.5,5000.5,0.)
      enddo

      do i=1,Nbarre
         call hbook1(29410+i,'ph_sipm (barre)',500,0.5,5000.5,0.)
      enddo



      
      call hbook2(10241,'profile baricentro',50,-8.,12.,50,-8.,12.,0.) ! hi/2d

      call hbook1(10042,'x_c',384,0.,10.,0.) !plot per event display
      call hbook1(10142,'x_c',384,-8.,12.,0.) !no reset
      
      call hbook1(10043,'y_c',384,0.,10.,0.)!plot per event display
      call hbook1(10143,'y_c',384,-8.,12.,0.) !no reset

      call hbook1(10242,'x_baric',384,-8.,12.,0.) !no reset
      call hbook1(10243,'y_baric',384,-8.,12.,0.) !no reset

      
      
      call hbook1(10044,'x_matrice',6,0.,10.,0.)
      call hbook1(10045,'y_matrice',6,0.,10.,0.)

!      call hbook1(10046,'deltax',50,-5.,5.,0.) !allineam Calo ai Si
!      call hbook1(10047,'deltay',50,-5.,5.,0.)
      
* allineam con baric log
      do k=0,12
         call hbook1(10046+100*k,'deltax',50,-5.,5.,0.) !allineam Calo ai Si
         call hbook1(10047+100*k,'deltay',50,-5.,5.,0.)
      enddo
      
**************
*     2cluster
      call hbook1(11,'2cluster',3,0.,3.,0.)
      call hbook1(12,'2cluster',3,0.,3.,0.)
      
      end

