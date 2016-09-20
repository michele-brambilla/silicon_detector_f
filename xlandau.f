      real function xlandau(x)
      
      common/pawpar/par(3)
*      real par(3)
      real lambda, xi
      xi = par(3)/4.02
      lambda = (x-par(2))/xi
*      landau=par(1)*exp(-.5*lambda*exp(-lambda))
      xlandau=par(1)*exp(-.5*(lambda+exp(-lambda)))
*      xlandau=par(1)*exp(-x)

*      landau=par(1)*exp(-.5*(x-par(2))/(par(3)/4.02)*exp(-(x-par(2))/
*     +     (par(3)/4.02)))

*      landau=par(1)*x*exp(-par(2)*x*x)

      return
      end
