****  ESC-X fortran-mode

      common /rawdata/raw
      common /data/subraw,imax
      common /cuts/pitch,x_conv,cut1,cut2
      common /xcluster/nrcluster,baricentro_mc,ampiezza_mc
      common /xpede/subpede,subrms,nmax,istat,istatus,xavg
      common /efficenza/trk_tot,trk_buone,trk_fallite
      common /angoli/betax,betay
      
*      parameter (start=4) ! le strip dalla V in avanti x Si
      integer,parameter :: nasic=3
      integer,parameter	:: nchan=128
      integer,parameter	:: nasic_b=5
      integer,parameter	:: nchan_b=64
      integer,parameter	:: Nstrip=384
      integer,parameter	:: Nsilicio=5
      integer,parameter	:: Maxcluster=1000
      real,parameter :: sigmax=0.1358
      real,parameter :: sigmay=0.1029

      character*50 output_dir

      integer istatus(Nstrip,Nsilicio)

      real raw(Nstrip,Nsilicio), subraw(Nstrip,Nsilicio) ! generali
      real subpede(Nstrip,Nsilicio),subrms(Nstrip,Nsilicio)
      real pitch(Nstrip,Nsilicio),x_conv(Nstrip,Nsilicio),cut1(Nsilicio)       ! taglio principale sul pull

      real betax,betay

      
      real xavg(Nstrip,Nsilicio) ! equalizzazione
      integer equal_si


      

      integer imax(Nsilicio)

      real cut2(Nsilicio)       ! taglio pull L/R

      real baricentro_mc(Nsilicio,Maxcluster) ! cluster usando baricentro
      real pull_mc(Nsilicio,Maxcluster) ! cluster usando pull

      real ampiezza_mc(Nsilicio,Maxcluster) ! per efficienza

      integer nrcluster(Nsilicio)

      integer istrip,isili,nmax,istat,fid ! varie

      integer trk_tot,trk_buone,trk_fallite ! efficenza

*****************
     
     
      integer lastevent
      
     
      integer do_display         ! dice di fare il displayevent
      
      common/display/do_display

***********************************************

**************basculo*****************
      integer vec_ROzone(20,2)
      
      common/basculo/vec_ROzone
