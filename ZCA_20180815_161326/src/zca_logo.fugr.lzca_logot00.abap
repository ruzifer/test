*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.07.2018 at 11:33:53
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_LOGO.......................................*
DATA:  BEGIN OF STATUS_ZTCA_LOGO                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_LOGO                     .
CONTROLS: TCTRL_ZTCA_LOGO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_LOGO                     .
TABLES: ZTCA_LOGO                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
