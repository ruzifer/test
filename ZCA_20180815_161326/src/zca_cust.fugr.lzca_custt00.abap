*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.05.2018 at 14:32:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_CUSTOBJ....................................*
DATA:  BEGIN OF STATUS_ZTCA_CUSTOBJ                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_CUSTOBJ                  .
CONTROLS: TCTRL_ZTCA_CUSTOBJ
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTCA_CUSTOBJ                  .
TABLES: ZTCA_CUSTOBJ                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
