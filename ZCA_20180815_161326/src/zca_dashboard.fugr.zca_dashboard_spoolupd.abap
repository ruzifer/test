FUNCTION ZCA_DASHBOARD_SPOOLUPD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MSGID) TYPE  SXMSGUID
*"     VALUE(IV_INFNAM) TYPE  ZCA_INTF_INFNAM
*"     VALUE(IV_DIRECTION) TYPE  ZCA_INTF_TYPE
*"     VALUE(IV_INTF_CAT) TYPE  ZCA_INTF_CATEGORY
*"     VALUE(IV_SENDER) TYPE  SXI_ADDRESS OPTIONAL
*"     VALUE(IV_RECEIVER) TYPE  SXI_ADDRESS OPTIONAL
*"     VALUE(IT_ACK) TYPE  ZTTCA_ACK
*"     VALUE(IV_COMMIT_NOW) TYPE  XFELD DEFAULT 'X'
*"----------------------------------------------------------------------
**  Write ACK to Spool
*   zcl_ca_dashboard=>write_spool( it_ack    = it_ack
*                                  iv_infnam = iv_infnam
*                                  iv_msgid  = iv_msgid ).

*  Update ACK to Database table
   zcl_ca_dashboard=>update_intf_log( iv_msgid     = iv_msgid
                                      iv_infnam    = iv_infnam
                                      iv_direction = iv_direction
                                      iv_intf_cat  = iv_intf_cat
                                      iv_sender    = iv_sender
                                      iv_receiver  = iv_receiver
                                      it_ack       = it_ack ).
   if iv_commit_now = abap_true.
     commit work.
   endif.
ENDFUNCTION.
