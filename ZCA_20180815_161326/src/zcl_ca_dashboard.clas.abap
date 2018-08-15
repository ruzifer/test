class ZCL_CA_DASHBOARD definition
  public
  final
  create public .

public section.

  class-methods GENERATE_SPOOL_FROM_PROXY
    importing
      value(IV_INFNAM) type ZCA_INTF_INFNAM
      value(IV_COMMIT_NOW) type ABAP_BOOL default 'X'
      value(IO_PROXY) type ref to OBJECT optional
      value(IV_MSGID) type SXMSGUID optional
      value(IV_RECEIVER) type SXI_ADDRESS optional
    changing
      value(CT_ACK) type ZTTCA_ACK
    raising
      CX_AI_SYSTEM_FAULT .
  class-methods GET_DASHBOARD_TIMESTAMP
    returning
      value(RV_TIMESTAMP) type ZSCA_ACK-START_TIME .
  class-methods UPDATE_INTF_LOG
    importing
      value(IV_MSGID) type SXMSGUID
      value(IV_INFNAM) type ZCA_INTF_INFNAM
      value(IV_DIRECTION) type ZCA_INTF_TYPE
      value(IV_INTF_CAT) type ZCA_INTF_CATEGORY
      value(IV_SENDER) type SXI_ADDRESS optional
      value(IV_RECEIVER) type SXI_ADDRESS optional
      value(IT_ACK) type ZTTCA_ACK .
protected section.
private section.

  types:
    CTY_INTF_MAP_TAB type table of ztca_intf_map
                         with key primary_key
                         components MESTYP IDOCTP CIMTYP MESFCT BUSINESSCOMP
                         with non-unique sorted key comp
                         components mestyp businesscomp .

  constants CC_SEP type ABAP_CHAR1 value '|' ##NO_TEXT.
  class-data ST_INTF_MAP type CTY_INTF_MAP_TAB .

  class-methods WRITE_JOB_LOG
    importing
      !IW_ACK type ZSCA_ACK
      !IV_PARTNER type SXI_ADDRESS optional
      !IV_INFNAM type ZCA_INTF_INFNAM optional .
  class-methods WRITE_SPOOL
    importing
      value(IV_MSGID) type SXMSGUID
      value(IV_INFNAM) type CHAR8
      value(IT_ACK) type ZTTCA_ACK .
ENDCLASS.



CLASS ZCL_CA_DASHBOARD IMPLEMENTATION.


  METHOD generate_spool_from_proxy.
*----------------------------------------------------------------------
    "Make sure there is acknowledgement to write and proxy class is available
    CHECK lines( ct_ack ) GT 0.
*----------------------------------------------------------------------
    "IT_ACK contains fields below but you need to populate only some field
    "  - LOCATION   = Populate with Store or leave blank if it's non-store specific
    "  - MSG_ID     = Leave blank, the message id will be populated inside this method
    "  - TOTAL      = Populate with Total number of record
    "  - SUCCESS    = Populate with Total number of successful record (should equal to Total)
    "  - ERROR      = Populate with Total number of error record (should be zero)
    "  - START_TIME = Leave blank, start time will be populated inside this method
*----------------------------------------------------------------------
    "If this is Outbound proxy (S4 -> SAP PO), Proxy object will be available
    "   Message ID and sent time will be on sender perspective
    "If this is Inbound proxy (SAP PO -> S4),
    "   we will use Message ID sent from SAP PO
    "   and populate only processing time... (get values outside this method)
    DATA:
      lo_protocol           TYPE REF TO if_wsprotocol,
      lo_protocol_messageid TYPE REF TO if_wsprotocol_message_id,
      lo_protocol_xi_header TYPE REF TO if_wsprotocol_xi_header,
      lo_protocol_routing   TYPE REF TO if_wsprotocol_routing,
      lo_srv_context        TYPE REF TO if_ws_server_context,
      lo_payload            TYPE REF TO if_ws_payload,   "cf. result of method “get_sent_request_payload”
      lv_message_id         TYPE sxmsguid,               "cf. result of method “get_message_id”
      lv_xml_string         TYPE string,                 "cf. result of method “get_xml_text”
      lv_time_n             TYPE n LENGTH 14,
      lv_time_c             TYPE c LENGTH 17,
      lv_timestamp          type tstmp,
      lv_timestamp_long     type TZNTSTMPL,
      lv_direction          TYPE ZCA_INTF_TYPE,
      lv_intf_cat           TYPE ZCA_INTF_CATEGORY,
      lv_sender             TYPE SXI_ADDRESS,
      lv_receiver           TYPE SXI_ADDRESS.
*----------------------------------------------------------------------
    clear: lv_sender, lv_receiver.
*----------------------------------------------------------------------
    "----- O U T B O U N D -----
    IF io_proxy IS SUPPLIED.
*     Interface direction (Outbound)
      lv_direction = 'OB'.

*     Get Protocol Class of parameter: Message_ID
      CALL METHOD io_proxy->('GET_PROTOCOL')
        EXPORTING
          protocol_name = if_wsprotocol=>message_id
        RECEIVING
          protocol      = lo_protocol.
      lo_protocol_messageid ?= lo_protocol.

*     Get Protocol Class of parameter: XI Header
      CALL METHOD io_proxy->('GET_PROTOCOL')
        EXPORTING
          protocol_name = if_wsprotocol=>xi_header
        RECEIVING
          protocol      = lo_protocol.
      lo_protocol_xi_header ?= lo_protocol.
*----------------------------------------------------------------------
*     Get Message ID
      if iv_msgid is supplied and iv_msgid is not initial.
        lv_message_id = iv_msgid.
      else.
        lv_message_id = lo_protocol_messageid->get_message_id( ).
      endif.

*     Get time sent from header in format yyyy-mm-ddThh:mm:ssZ
      try.
          DATA(lv_time_sent) = lo_protocol_xi_header->get_header_field( fieldname = if_wsprotocol_xi_header=>time_sent ).
        catch CX_AI_SYSTEM_FAULT.
          convert date sy-datum
                  time sy-uzeit
             into time stamp data(lv_tstmp_sent)
             time zone 'UTC'.
          lv_time_sent = lv_tstmp_sent.
      endtry.

*     Receiver party
      lv_receiver = iv_receiver.

      IF lv_time_sent CP '++++-++-++T++:++:++Z'.
        lv_time_n = lv_time_sent.
        TRY.
          clear: lv_timestamp, lv_timestamp_long.
          lv_timestamp = lv_time_n.
          lv_timestamp_long = cl_abap_tstmp=>add( EXPORTING tstmp = lv_timestamp
                                                            secs  = '25200' ).  "Add 7 hours (to UTC+7)
          lv_time_n = lv_timestamp_long.

           CATCH cx_parameter_invalid_range
                 cx_parameter_invalid_type
                 cx_sy_conversion_no_number.
             lv_time_n = lv_time_sent.
        ENDTRY.

        "In predefined format, populate them
        lv_time_c = |{ lv_time_n }000|.
      ELSE.
        "Not in predefined format, populate as is
        lv_time_c = lv_time_sent.
      ENDIF.
*----------------------------------------------------------------------
*     Populate Message ID and Time sent to the ACK
      LOOP AT ct_ack ASSIGNING FIELD-SYMBOL(<lf_ack>).
        <lf_ack>-msg_id     = lv_message_id.
        <lf_ack>-start_time = lv_time_c.
        write_job_log( iw_ack     = <lf_ack>
                       iv_infnam  = iv_infnam
                       iv_partner = lv_receiver ).
      ENDLOOP.
*----------------------------------------------------------------------
      "----- I N B O U N D -----
    ELSE.
      TRY.
*         Interface direction (Inbound)
          lv_direction = 'IB'.

          lo_srv_context         = cl_proxy_access=>get_server_context( ).

          "Get Message GUID
          lo_protocol_messageid ?= lo_srv_context->get_protocol( if_wsprotocol=>message_id ).
          if iv_msgid is supplied and iv_msgid is not initial.
            lv_message_id = iv_msgid.
          else.
            lv_message_id = lo_protocol_messageid->get_message_id( ).
          endif.

          "Get Routing
          lo_protocol_routing ?= lo_srv_context->get_protocol( if_wsprotocol=>routing ).
          lv_sender = lo_protocol_routing->get_sender( ).

*----------------------------------------------------------------------
*         Populate Message ID and Time sent to the ACK
          LOOP AT ct_ack ASSIGNING <lf_ack>.
            if <lf_ack>-msg_id is initial.
              <lf_ack>-msg_id     = lv_message_id.
            endif.
            write_job_log( iw_ack     = <lf_ack>
                           iv_infnam  = iv_infnam
                           iv_partner = lv_sender ).
*            <lf_ack>-start_time = lv_time_c.
          ENDLOOP.
*----------------------------------------------------------------------
        CATCH cx_ai_system_fault INTO DATA(lo_error).
          "In case of any error, update the value as-is
      ENDTRY.
    ENDIF.
*----------------------------------------------------------------------
    lv_intf_cat = 'AP'.  "ABAP Proxy
*----------------------------------------------------------------------
*  Write ACK to Spool
    "Use DESTINATION NONE to avoid COMMIT WORK in the ABAP Proxy program
    CALL FUNCTION 'ZCA_DASHBOARD_SPOOLUPD'
      DESTINATION 'NONE'
      EXPORTING
        iv_msgid      = lv_message_id
        iv_infnam     = iv_infnam
        iv_direction  = lv_direction
        iv_sender     = lv_sender
        iv_receiver   = lv_receiver
        iv_intf_cat   = lv_intf_cat
        it_ack        = ct_ack
        iv_commit_now = iv_commit_now.
*----------------------------------------------------------------------
  ENDMETHOD.


  METHOD GET_DASHBOARD_TIMESTAMP.
    "This method try to format time stamp to ddMMyyyyHHmmSSsssss
    data: lv_timestamp type timestampl.

    get TIME STAMP FIELD lv_timestamp.
    data(lv_temp) = |{ lv_timestamp TIMESTAMP = SPACE
                                    TIMEZONE  = 'UTC+7' }|. "always use UTC+7 for dashboard
    lv_temp = translate( val = lv_temp from = ':.-' to = '' ).
    condense lv_temp NO-GAPS.

    rv_timestamp = lv_temp(17).
  ENDMETHOD.


  METHOD update_intf_log.
    DATA: lt_intf_log TYPE TABLE OF ztca_intf_log,
          lw_intf_log TYPE ztca_intf_log,
          lw_intf_map TYPE ztca_intf_map,
          lt_intf_map TYPE cty_intf_map_tab,
          lv_date     TYPE sy-datum,
          lv_time     TYPE sy-uzeit,
          lv_msgtyp   TYPE edi_mestyp.

    "Initialize value for this update
    lv_date   = sy-datum.
    lv_time   = sy-uzeit.
    lv_msgtyp = iv_infnam.

    IF lines( st_intf_map ) EQ 0.
      "Load interface mapping table into memory
      SELECT * FROM ztca_intf_map INTO TABLE st_intf_map ORDER BY PRIMARY KEY.
    ENDIF.

    CLEAR lt_intf_log.

    LOOP AT it_ack INTO DATA(lw_ack).
      CLEAR: lw_intf_log, lw_intf_map, lt_intf_map.
      lw_intf_log-zzintfid        = lw_ack-msg_id.
      lw_intf_log-zztype          = iv_direction.      "OB (Outbound); IB (Inbound)
      lw_intf_log-zzcate          = iv_intf_cat.       "ID (IDoc); AP (ABAP Proxy)
      lw_intf_log-zzstore         = lw_ack-location.   "Store
      lw_intf_log-zzmessage_type  = lv_msgtyp.         "Message Type
      lw_intf_log-zztotal_record  = lw_ack-total.      "Total Record
      lw_intf_log-zztotal_success = lw_ack-success.    "Total success record
      lw_intf_log-zztotal_error   = lw_ack-error.      "Total error record
      lw_intf_log-zzother_ack     = lw_ack-start_time. "Timestamp.
      lw_intf_log-ernam           = sy-uname.
      lw_intf_log-erdat           = lv_date.
      lw_intf_log-erzet           = lv_time.

      "Map from IV_INFNAM to ZMODULE + ZBUS_OBJ
      IF iv_sender IS INITIAL AND iv_receiver IS INITIAL.
        "No SENDER / RECEIVER passing in
        "(probably it is normal outbound interface (no explicit receiver info in payload)), select all
        lt_intf_map = FILTER cty_intf_map_tab( st_intf_map USING KEY comp
                                               WHERE mestyp       = lv_msgtyp ).
      ELSEIF iv_sender IS NOT INITIAL.
        "Sender passing in (update only the specified SENDER)
        lt_intf_map = FILTER cty_intf_map_tab( st_intf_map USING KEY comp
                                               WHERE mestyp       = lv_msgtyp
                                                 AND businesscomp = conv ZCA_INTF_RECEIVER( iv_sender-service ) ).
      ELSEIF iv_receiver IS NOT INITIAL.
        "Receiver passing in (update only the specified RECEIVER)
        lt_intf_map = FILTER cty_intf_map_tab( st_intf_map USING KEY comp
                                               WHERE mestyp       = lv_msgtyp
                                                 AND businesscomp = conv ZCA_INTF_RECEIVER( iv_receiver-service ) ).
      ENDIF.

      IF lines( lt_intf_map ) GT 0.
        "Update all relevant SENDER / RECEIVER
        LOOP AT lt_intf_map INTO lw_intf_map.
          lw_intf_log-zztype     = lw_intf_map-zztype.  "OB (Outbound); IB (Inbound)
          lw_intf_log-zzcate     = lw_intf_map-zzcate.  "ID (IDoc); AP (ABAP Proxy)
          lw_intf_log-zzmodule   = lw_intf_map-zzmodule.
          lw_intf_log-zzreceiver = lw_intf_map-businesscomp.
          lw_intf_log-zzbus_obj  = lw_intf_map-zzbus_obj.
          APPEND lw_intf_log TO lt_intf_log.
        ENDLOOP.
      ELSE.
        "No interface mapping found, just update with IV_INFNAM
        "This entry might be unusable but it is telling us that the mapping is missing
        lw_intf_log-zzmodule  = space.
        lw_intf_log-zzbus_obj = iv_infnam.
        APPEND lw_intf_log TO lt_intf_log.
      ENDIF.
    ENDLOOP.

    IF lines( lt_intf_log ) GT 0.
      MODIFY ztca_intf_log FROM TABLE lt_intf_log.
    ENDIF.
  ENDMETHOD.


  method WRITE_JOB_LOG.
    "Write in SM37 job log only when it is triggered in background mode
    check sy-batch is not initial.

    "Get mapping table (if not exist)
    IF lines( st_intf_map ) EQ 0.
      SELECT * FROM ztca_intf_map
        INTO TABLE st_intf_map
        ORDER BY PRIMARY KEY.
    ENDIF.

    data: lt_intf_map TYPE cty_intf_map_tab,
          lv_msgtyp   TYPE edi_mestyp.

    lv_msgtyp = iv_infnam.

    "Map from IV_INFNAM to ZMODULE + ZBUS_OBJ
    IF iv_partner IS INITIAL.
      "No sender/receiver passing in (probably it is outbound interface), select all
      lt_intf_map = FILTER cty_intf_map_tab( st_intf_map USING KEY comp
                                             WHERE mestyp       = lv_msgtyp ).
    ELSE.
      "Sender/Receiver passing in (update only the specified partner)
      lt_intf_map = FILTER cty_intf_map_tab( st_intf_map USING KEY comp
                                             WHERE mestyp       = lv_msgtyp
                                               AND businesscomp = conv ZCA_INTF_RECEIVER( iv_partner-service ) ).
    ENDIF.

    "Trigger in background, write jog lob
    loop at lt_intf_map into data(lw_intf_map).
      message s080(zca00) with iw_ack-location lw_intf_map-zzbus_obj iw_ack-msg_id.
    endloop.
  endmethod.


  METHOD write_spool.
    DATA: ls_params TYPE pri_params,
          lv_valid.
*    CONSTANTS: cc_sep type ABAP_CHAR1 value '|'.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        LIST_NAME          = conv PRI_PARAMS-PLIST( |ACK_{ iv_infnam }| )
        NO_DIALOG          = 'X'
        NEW_LIST_ID        = 'X'
        PROTECT_LIST       = 'X'
        DESTINATION        = 'LP01'
        LIST_TEXT          = conv PRI_PARAMS-PRTXT( |ACK_{ iv_msgid }| )
      IMPORTING
        out_parameters     = ls_params
        valid              = lv_valid
      EXCEPTIONS
        OTHERS             = 1.
    check sy-subrc = 0.

    NEW-PAGE PRINT ON NO-TITLE NO-HEADING PARAMETERS ls_params NO DIALOG.
    loop at it_ack into data(lw_ack).
      write:/ |{ lw_ack-LOCATION }{ cc_sep }{ lw_ack-MSG_ID }{ cc_sep }{ lw_ack-TOTAL }{ cc_sep }|
           && |{ lw_ack-SUCCESS }{ cc_sep }{ lw_ack-ERROR }{ cc_sep }{ lw_ack-START_TIME }|.
    endloop.
    NEW-PAGE PRINT OFF.
  ENDMETHOD.
ENDCLASS.
