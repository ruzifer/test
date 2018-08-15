FUNCTION zca_intf_read_acknowledge_all .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INTF_ID) TYPE  ZCA_INTF_GUID OPTIONAL
*"     VALUE(IV_INTF_BUS_OBJ) TYPE  ZCA_INTF_BUS_OBJ OPTIONAL
*"     VALUE(IV_INTF_TYPE) TYPE  ZCA_INTF_TYPE OPTIONAL
*"     VALUE(IV_INTF_CATE) TYPE  ZCA_INTF_CATEGORY OPTIONAL
*"     VALUE(IV_INTF_MODULE) TYPE  ZCA_INTF_MODULE OPTIONAL
*"  TABLES
*"      ET_INTF_LOG STRUCTURE  ZSCA_INTF_ACKNOWLEDGE OPTIONAL
*"      ET_INTF_ACKNOWLEDGE STRUCTURE  ZSCA_INTF_ACKNOWLEDGE OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
*   There are 2 ways of selecting interface log
*   1) Use primary key   : ZID
*   2) Use supplement key: ZTYPE, ZCATE, ZMODULE, ZBUS_OBJ
*-----------------------------------------------------------------------
  REFRESH: et_intf_log, et_intf_acknowledge.

  DATA: lt_intf_log    TYPE TABLE OF ztca_intf_log,
        lw_intf_log    TYPE ztca_intf_log,
        lw_acknowledge TYPE zsca_intf_acknowledge.

  IF iv_intf_id IS NOT INITIAL.
    "Select with primary key
    IF iv_intf_bus_obj IS INITIAL.
      SELECT *
        INTO TABLE lt_intf_log
        FROM ztca_intf_log
       WHERE zzintfid   EQ iv_intf_id          "IDoc generated ID (customized); ABAP Proxy GUID
         AND zzcomplete EQ abap_false.
    ELSE.
      SELECT *
        INTO TABLE lt_intf_log
        FROM ztca_intf_log
       WHERE zzintfid   EQ iv_intf_id          "IDoc generated ID (customized); ABAP Proxy GUID
         AND zzbus_obj  EQ iv_intf_bus_obj
         AND zzcomplete EQ abap_false.
    ENDIF.
  ELSE.
    "Select with supplement key
    SELECT *
      INTO TABLE lt_intf_log
      FROM ztca_intf_log
     WHERE zztype     EQ iv_intf_type     "IB (Inbound); OB (Outbound)
       AND zzcate     EQ iv_intf_cate     "ID (IDoc), AP (ABAP Proxy)
       AND zzmodule   EQ iv_intf_module   "Module
       AND zzbus_obj  EQ iv_intf_bus_obj  "Sub-business object
       AND zzcomplete EQ abap_false.

  ENDIF.

  IF lt_intf_log[] IS INITIAL.
    RAISE no_data_found.
  ELSE.
    LOOP AT lt_intf_log INTO lw_intf_log.
      IF lw_intf_log-zztype EQ 'IB' AND lw_intf_log-zzcate EQ 'ID'.
* in case of IB (Inbound) and  ID (IDoc), return Idoc number
        lw_acknowledge-acknowledge = |{ lw_intf_log-zzstore }\|{ lw_intf_log-zzmessage_type }\|{ lw_intf_log-zzdocnum }|.
      ELSE.
        lw_acknowledge-acknowledge = |{ lw_intf_log-zzstore }\|{ lw_intf_log-zzmessage_type }\|{ lw_intf_log-zzintfid }|.
      ENDIF.
      APPEND lw_acknowledge TO et_intf_log.

      lw_acknowledge-acknowledge = |{ lw_intf_log-zzstore }\|{ lw_intf_log-zzintfid }\|{ lw_intf_log-zztotal_record }\||
                                && |{ lw_intf_log-zztotal_success }\| { lw_intf_log-zztotal_error }\| { lw_intf_log-zzother_ack }|.
      APPEND lw_acknowledge TO et_intf_acknowledge.

*      lw_intf_log-zzcomplete = abap_true.
      MODIFY ztca_intf_log FROM @lw_intf_log.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
