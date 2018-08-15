*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.01.2018 at 15:39:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_VARV.......................................*
DATA:  BEGIN OF STATUS_ZTCA_VARV                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_VARV                     .
CONTROLS: TCTRL_ZTCA_VARV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_VARV                     .
TABLES: ZTCA_VARV                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
