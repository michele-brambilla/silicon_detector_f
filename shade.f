      subroutine shade()
      parameter (nmx = 20)
      common /shpt/ npt,idx(nmx),r(nmx),g(nmx),b(nmx)
      if (npt.lt.2) then
         print*, 'Error: at least two colours are needed'
         return
      endif
      do i=2,npt
         j  = i-1
         i1 = idx(j)
         i2 = idx(i)
         r1 = r(j)
         g1 = g(j)
         b1 = b(j)
         r2 = r(i)
         g2 = g(i)
         b2 = b(i)
         n  = i2-i1+1
         do ii=i1,i2
            scale = float(ii-i1)/(n-1)
            rs = (r2 - r1)*scale + r1
            gs = (g2 - g1)*scale + g1
            bs = (b2 - b1)*scale + b1
            call iscr(1,ii,rs,gs,bs)
         enddo
      enddo
      end

