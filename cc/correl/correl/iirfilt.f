                                                            
C  ARGUMENTS:                                                                   
C  ----------                                                                   
C                                                                               
C    DATA           REAL ARRAY CONTAINING SEQUENCE TO BE FILTERED               
C                     ORIGINAL DATA DESTROYED, REPLACED BY FILTERED DATA        
C                                                                               
C    NSAMPS         NUMBER OF SAMPLES IN DATA                                   
C                                                                               
C                                                                               
C    APROTO         CHARACTER*8 VARIABLE, CONTAINS TYPE OF ANALOG               
C                     PROTOTYPE FILTER                                          
C                     '(BU)TTER  ' -- BUTTERWORTH FILTER                        
C                     '(BE)SSEL  ' -- BESSEL FILTER                             
C                                                                               
C    IORD           ORDER (#POLES) OF ANALOG PROTOTYPE                          
C                   NOT TO EXCEED 10 IN THIS CONFIGURATION.  4 - 5              
C                   SHOULD BE AMPLE.                                            
C                                                                               
C    TYPE           CHARACTER*8 VARIABLE CONTAINING FILTER TYPE                 
C                     'LP' -- LOW PASS                                          
C                     'HP' -- HIGH PASS                                         
C                     'BP' -- BAND PASS                                         
C                     'BR' -- BAND REJECT                                       
C                                                                               
C    FLO            LOW FREQUENCY CUTOFF OF FILTER (HERTZ)                      
C                   IGNORED IF TYPE = 'LP'                                      
C                                                                               
C    FHI            HIGH FREQUENCY CUTOFF OF FILTER (HERTZ)                     
C                   IGNORED IF TYPE = 'HP'                                      
C                                                                               
C    TS             SAMPLING INTERVAL (SECONDS)                                 
C                                                                               
C    PASSES           INTEGER VARIABLE CONTAINING THE NUMBER OF PASSES          
C                   1 -- FORWARD FILTERING ONLY                                 
C                   2 -- FORWARD AND REVERSE (I.E. ZERO PHASE) FILTERING        
C                                                                               
C                                                                               
C  SUBPROGRAMS REFERENCED:  BILIN2, BUROOTS, WARP, CUTOFFS, LPTHP, LPTBP,       
C    LP, LPTBR, BEROOTS, DESIGN, APPLY              
C                                                                               
      SUBROUTINE IIRFILT( DATA, NSAMPS, APROTO, IORD,         
     +                   TYPE, FLO, FHI, TS, PASSES )                   
C                                                                               
 
        DIMENSION DATA(1)                                               
        CHARACTER*8 TYPE, APROTO                                        
        INTEGER NSAMPS, PASSES, IORD                                    
        REAL*4 FLO, FHI, TS, SN(30), SD(30)                  
        LOGICAL ZP                                                      
C                                                                               
C  Filter designed                                                              
C                                                                               
        CALL DESIGN( IORD, TYPE(1:2), APROTO(1:2),            
     &               FLO, FHI, TS, SN, SD, NSECTS )                     
C                                                                               
C  Filter data                                                                  
C                                                                               
        IF (   PASSES .EQ. 1 ) THEN                                     
          ZP = .FALSE.                                                  
        ELSE                                                            
          ZP = .TRUE.                                                   
        END IF                                                          
        CALL APPLY( DATA, NSAMPS, ZP, SN, SD, NSECTS )                  
C                                                                               
      RETURN                                                            
      END                                                               
