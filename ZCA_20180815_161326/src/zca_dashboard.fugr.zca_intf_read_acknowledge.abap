FUNCTION ZCA_INTF_READ_ACKNOWLEDGE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INTF_ID) TYPE  ZCA_INTF_GUID OPTIONAL
*"     VALUE(IV_INTF_BUS_OBJ) TYPE  ZCA_INTF_BUS_OBJ OPTIONAL
*"     VALUE(IV_INTF_TYPE) TYPE  ZCA_INTF_TYPE OPTIONAL
*"     VALUE(IV_INTF_CATE) TYPE  ZCA_INTF_CATEGORY OPTIONAL
*"     VALUE(IV_INTF_MODULE) TYPE  ZCA_INTF_MODULE OPTIONAL
*"  EXPORTING
*"     VALUE(EV_INTF_LOG) TYPE  ZCA_INTF_ACKNOWLEDGE
*"     VALUE(EV_INTF_ACKNOWLEDGE) TYPE  ZCA_INTF_ACKNOWLEDGE
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
*   There are 2 ways of selecting interface log
*   1) Use primary key   : ZID
*   2) Use supplement key: ZTYPE, ZCATE, ZMODULE, ZBUS_OBJ
*-----------------------------------------------------------------------
  CLEAR: ev_intf_log, ev_intf_acknowledge.

  DATA: lw_intf_log TYPE ztca_intf_log.

  IF iv_intf_id IS NOT INITIAL.
    "Select with primary key
    IF iv_intf_bus_obj IS INITIAL.
      SELECT *
        UP TO 1 ROWS
        INTO lw_intf_log
        FROM ztca_intf_log
       WHERE zzintfid   EQ iv_intf_id          "IDoc generated ID (customized); ABAP Proxy GUID
         AND zzcomplete EQ abap_false.
      ENDSELECT.
    ELSE.
      SELECT SINGLE *
        INTO lw_intf_log
        FROM ztca_intf_log
       WHERE zzintfid   EQ iv_intf_id          "IDoc generated ID (customized); ABAP Proxy GUID
         AND zzbus_obj  EQ iv_intf_bus_obj
         AND zzcomplete EQ abap_false.
    ENDIF.
  ELSE.
    "Select with supplement key
    SELECT *
    UP TO 1 ROWS
    INTO lw_intf_log
    FROM ztca_intf_log
   WHERE zztype     EQ iv_intf_type     "IB (Inbound); OB (Outbound)
     AND zzcate     EQ iv_intf_cate     "ID (IDoc), AP (ABAP Proxy)
     AND zzmodule   EQ iv_intf_module   "Module
     AND zzbus_obj  EQ iv_intf_bus_obj  "Sub-business object
     AND zzcomplete EQ abap_false.
    ENDSELECT.
  ENDIF.

  IF lw_intf_log IS INITIAL.
    RAISE no_data_found.
  ELSE.
    ev_intf_log         = |{ lw_intf_log-zzstore }\|{ lw_intf_log-zzmessage_type }\|{ lw_intf_log-zzintfid }|.
    ev_intf_acknowledge = |{ lw_intf_log-zzstore }\|{ lw_intf_log-zzintfid }\|{ lw_intf_log-zztotal_record }\||
                       && |{ lw_intf_log-zztotal_success }\| { lw_intf_log-zztotal_error }\| { lw_intf_log-zzother_ack }|.

    lw_intf_log-zzcomplete = abap_true.
    MODIFY ztca_intf_log FROM @lw_intf_log.
  ENDIF.
ENDFUNCTION.
