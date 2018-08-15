*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.01.2018 at 15:58:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_TCURT_WORD.................................*
DATA:  BEGIN OF STATUS_ZTCA_TCURT_WORD               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_TCURT_WORD               .
CONTROLS: TCTRL_ZTCA_TCURT_WORD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_TCURT_WORD               .
TABLES: ZTCA_TCURT_WORD                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
