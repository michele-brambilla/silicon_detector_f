SHELL = /bin/zsh
AR=ar rcs

.SUFFIXES:
.SUFFIXES: .f .o

CXXFLAGS = -DDO_DEBUG -ggdb -O0
FFLAGS = -ggdb -O0

LIBS  = `cernlib packlib pawlib graflib`
LDFLAGS = -L/usr/lib

INCLUDE = -Wcpp -Irapidjson/include


CXX = g++-4.8 -std=c++11 --fast-math -Wno-cpp -Df2cFortran
FF = gfortran-mp-4.8 -Df2cFortran

headers = common.inc variabili.inc

objects = sig_si.o status.o pull.o prepara_histo_si.o \
	 basculo.o cluster.o cluster_basculo.o

mains = cernrun_Si.o pedestal.o pedestalnosub.o

exe = $(patsubst %.o,%,$(mains))

all: $(exe)

$(objects): %.o : %.f $(headers)
	$(FF) $(FFLAGS) -c $< -I.

$(mains): %.o : %.f $(headers)
	$(FF) $(FFLAGS) -c $< -I.

$(exe): $(mains) $(objects) $(headers)
	$(FF) $(LDFLAGS) -o $@ $@.o $(objects) $(LIBS)

.PHONY : clean


clean:
	rm *.o cernrun_Si pedestal pedestalnosub
