c preprocess.inc: Stores parameters that define array dimensions
c            Modify to fit size of problem and available computer memory.
c Parameter Description:
c MAXSTA:  Max number of stations for new event.
c MAXEVE:   Max number of events in catalog.
c MAXFILES:  Max number of files at a given station.
c MAXLAY: Max number of layers in velocity model


      integer*4 MAXEVE,MAXSTA,MAXLAY,MAXFILES

      parameter(MAXSTA=1000,
     & 		MAXEVE=500000,
     &          MAXFILES=500000,
     &          MAXLAY=70)
