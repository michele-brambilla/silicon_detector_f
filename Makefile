SHELL = /bin/zsh
AR=ar rcs

.SUFFIXES:
.SUFFIXES: .f .o

CXXFLAGS = -DDO_DEBUG -ggdb -O0
FFLAGS = -ggdb -O0

LIBS  = `cernlib -safe packlib pawlib`
LDFLAGS = -L/usr/lib

INCLUDE = -Wcpp -Irapidjson/include


CXX = g++-4.8 -std=c++11 --fast-math -Wno-cpp -Df2cFortran
FF = gfortran-4.8 -Df2cFortran

headers = common.inc variabili.inc

objects = sig_si.o status.o pull.o prepara_histo_si.o \
	 basculo.o cluster.o cluster_basculo.o

mains = cernrun_Si.o pedestal.o pedestalnosub.o pedestalsub.o

exe = $(patsubst %.o,%,$(mains))

all: $(exe)

$(objects): %.o : %.f $(headers)
	$(FF) $(FFLAGS) -c $< -I.

$(mains): %.o : %.f $(headers)
	$(FF) $(FFLAGS) -c $< -I.

$(exe): $(mains) $(objects) $(headers)
	$(FF) $(LDFLAGS) -o $@ $@.o $(objects) $(LIBS)

.PHONY : clean


#cernrun_Si: cernrun_Si.f $(src) sig_si.f common.inc variabili.inc
#	gfortran $(OPT) -o cernrun_Si cernrun_Si.f $(src) sig_si.f $(LIBS) $(FLAG)
#
#pedestal: pedestal.f common.inc
#	gfortran $(OPT) -o pedestal pedestal.f $(LIBS) $(FLAG)
#
#pedestal1: pedestal1.f common.inc
#	gfortran $(OPT) -o pedestal1 pedestal1.f $(LIBS) $(FLAG)
#
#pedestalnosub: pedestalnosub.f common.inc
#	gfortran $(OPT) -o pedestalnosub pedestalnosub.f $(LIBS) $(FLAG)
#
#pedestalsub: pedestalsub.f common.inc
#	gfortran $(OPT) -o pedestalsub pedestalsub.f $(LIBS) $(FLAG)
#
clean:
	rm *.o cernrun_Si pedestal
