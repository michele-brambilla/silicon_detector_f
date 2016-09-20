      subroutine setshd(idxi, ri, gi, bi)
      parameter (nmx = 20)
      common /shpt/ npt,idx(nmx),r(nmx),g(nmx),b(nmx)
      if (idxi.lt.0) then
         npt = 0
         return
      endif
      npt = npt+1
      if (npt.gt.nmx) then
         print*, 'Error: too many colours'
         return
      endif
      idx(npt) = idxi
      r(npt)   = ri
      g(npt)   = gi
      b(npt)   = bi
      end

