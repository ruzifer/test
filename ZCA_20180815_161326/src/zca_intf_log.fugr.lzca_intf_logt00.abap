*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.08.2018 at 15:06:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_INTF_LOG...................................*
DATA:  BEGIN OF STATUS_ZTCA_INTF_LOG                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_INTF_LOG                 .
CONTROLS: TCTRL_ZTCA_INTF_LOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTCA_INTF_MAP...................................*
DATA:  BEGIN OF STATUS_ZTCA_INTF_MAP                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_INTF_MAP                 .
CONTROLS: TCTRL_ZTCA_INTF_MAP
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZTCA_INTF_SYS...................................*
DATA:  BEGIN OF STATUS_ZTCA_INTF_SYS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_INTF_SYS                 .
CONTROLS: TCTRL_ZTCA_INTF_SYS
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZTCA_INTF_SYSID.................................*
DATA:  BEGIN OF STATUS_ZTCA_INTF_SYSID               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_INTF_SYSID               .
CONTROLS: TCTRL_ZTCA_INTF_SYSID
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZVCA_INTF_INFO..................................*
TABLES: ZVCA_INTF_INFO, *ZVCA_INTF_INFO. "view work areas
CONTROLS: TCTRL_ZVCA_INTF_INFO
TYPE TABLEVIEW USING SCREEN '0011'.
DATA: BEGIN OF STATUS_ZVCA_INTF_INFO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVCA_INTF_INFO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVCA_INTF_INFO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVCA_INTF_INFO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCA_INTF_INFO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVCA_INTF_INFO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVCA_INTF_INFO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCA_INTF_INFO_TOTAL.

*...processing: ZVCA_INTF_INFOST................................*
TABLES: ZVCA_INTF_INFOST, *ZVCA_INTF_INFOST. "view work areas
CONTROLS: TCTRL_ZVCA_INTF_INFOST
TYPE TABLEVIEW USING SCREEN '0012'.
DATA: BEGIN OF STATUS_ZVCA_INTF_INFOST. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVCA_INTF_INFOST.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVCA_INTF_INFOST_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVCA_INTF_INFOST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCA_INTF_INFOST_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVCA_INTF_INFOST_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVCA_INTF_INFOST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVCA_INTF_INFOST_TOTAL.

*.........table declarations:.................................*
TABLES: *ZTCA_INTF_LOG                 .
TABLES: *ZTCA_INTF_MAP                 .
TABLES: *ZTCA_INTF_SYS                 .
TABLES: *ZTCA_INTF_SYSID               .
TABLES: T001W                          .
TABLES: ZTCA_INTF_INFO                 .
TABLES: ZTCA_INTF_INFOST               .
TABLES: ZTCA_INTF_LOG                  .
TABLES: ZTCA_INTF_MAP                  .
TABLES: ZTCA_INTF_SYS                  .
TABLES: ZTCA_INTF_SYSID                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
