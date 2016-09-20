
LIBS =-lpawlib -lmathlib -lgraflib -lgrafX11 -lpacklib -lX11 -L/sw/lib

OPT=-m64 # -Wall

FLAG=-ggdb -g

src=status.f prepara_histo_si.f pull.f basculo.f cluster.f cluster_basculo.f

all: cernrun_Si

cernrun_Si: cernrun_Si.f $(src) sig_si.f common.inc variabili.inc
	gfortran $(OPT) -o cernrun_Si cernrun_Si.f $(src) sig_si.f $(LIBS) $(FLAG)

pedestal: pedestal.f common.inc
	gfortran $(OPT) -o pedestal pedestal.f $(LIBS) $(FLAG)

pedestal1: pedestal1.f common.inc
	gfortran $(OPT) -o pedestal1 pedestal1.f $(LIBS) $(FLAG)

pedestalnosub: pedestalnosub.f common.inc
	gfortran $(OPT) -o pedestalnosub pedestalnosub.f $(LIBS) $(FLAG)

pedestalsub: pedestalsub.f common.inc
	gfortran $(OPT) -o pedestalsub pedestalsub.f $(LIBS) $(FLAG)

clean:
	rm *.o cernrun_Si pedestal
