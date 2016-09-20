      program status_pedestal

      include '../variabili.inc'

      LOGICAL :: file_exists
      character*100 in
      character*100 nomefile

      nomefile="raw/"
      call getarg(1, in)
      ll=len_trim(in)
      if(ll.ne.0) then
         nomefile(5:5+ll) = trim(in)
      else
         nomefile="in.dat"
      endif
      
      INQUIRE(FILE=nomefile, EXIST=file_exists)
      if (file_exists) then
         print *,"trovato file di pede"
      end if
      
c     leggo l'output formattato di pede, rms
      open(unit=10,file=in,form='formatted',action
     $     ="read")

*     devo usare subraw e subrms perche' sono gli oggetti di dimensione
*     giusta gia' presenti in variabili.inc: in realta' quello che vado
*     a leggere sono pede e rms
      
      do isilicio=1,5
         do istrip=1,384
            read(10,'(2(F15.5))'),subraw(istrip,isilicio),subrms(istrip
     $           ,isilicio)
         end do
      end do

      call get_status

      end program status_pedestal
      
