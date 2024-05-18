c correl.inc: Stores parameters that define array dimensions in pairs.
c            Modify to fit size of problem and available computer memory.
c Parameter Description:
c MAXEVE:   Max number of events.
c MAXSTA:  Max number of stations.
c MAXPAIR:  Max number of pairs.
c MAXSAMP:  Max number of sample in seismogram


      integer*4	MAXEVE, MAXSTA, MAXPAIR,MAXSAMP,MAXFILES
      parameter(MAXEVE=1068770,
     &          MAXSTA=2,
     &          MAXPAIR=106725463,  ! inactive
     &          MAXSAMP=402,
     &		MAXFILES=334839)

c NC.JST
c      parameter(MAXEVE=77996,
c     &          MAXSTA=2,
c     &          MAXPAIR=71719190,
c     &          MAXSAMP=401,
c     &		MAXFILES=54733)
c NC.MDR
c      parameter(MAXEVE=368500,
c     &          MAXSTA=2,
c     &          MAXPAIR=106725463,  ! inactive
c     &          MAXSAMP=501,
c     &		MAXFILES=145950)
