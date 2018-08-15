FUNCTION zca_intf_create_record_id .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INTF_ID) TYPE  ZCA_INTF_GUID OPTIONAL
*"     VALUE(IV_INTF_TYPE) TYPE  ZCA_INTF_TYPE
*"     VALUE(IV_INTF_CATE) TYPE  ZCA_INTF_CATEGORY
*"     VALUE(IV_INTF_MODULE) TYPE  ZCA_INTF_MODULE
*"     VALUE(IV_INTF_BUS_OBJ) TYPE  ZCA_INTF_BUS_OBJ
*"     VALUE(IV_INTF_BUSCOMP) TYPE  ZCA_INTF_RECEIVER
*"     VALUE(IV_MESTYP) TYPE  EDI_MESTYP OPTIONAL
*"     VALUE(IV_PARTNER) TYPE  EDIPPARNUM OPTIONAL
*"     VALUE(IV_SYSID) TYPE  SYST_SYSID OPTIONAL
*"     VALUE(IV_DOCNUM) TYPE  EDI_DOCNUM OPTIONAL
*"     VALUE(IT_INTF_ACK) TYPE  ZTTCA_ACK OPTIONAL
*"  CHANGING
*"     VALUE(CH_INTF_ID) TYPE  ZCA_INTF_GUID OPTIONAL
*"  EXCEPTIONS
*"      GENERATE_ID_ERROR
*"----------------------------------------------------------------------

  DATA: lv_intf_id      TYPE zca_intf_guid,
        lv_cate_ap      TYPE zca_intf_category,
        lv_cate_id      TYPE zca_intf_category,
        lv_type_ib      TYPE zca_intf_type,
        lv_type_ob      TYPE zca_intf_type,
        lt_intf_log     TYPE zttca_intf_log,
*        lt_intf_log_ref TYPE zttca_intf_log,  "Existing entry from db
        lt_intf_ack     TYPE zttca_ack,
        lv_subobj       TYPE nrsobj,
        lv_date         TYPE sy-datum,
        lv_time         TYPE sy-uzeit,
        lv_sysid        TYPE syst_sysid,
        lv_tstmp        TYPE tstmp,
        lw_intf_log     TYPE ztca_intf_log.

*  CLEAR lt_intf_log_ref.

* Get constant value
  zcl_ca_utility=>read_variable_value(
      EXPORTING iv_zzprog  = 'ZCA_INTERFACE_RECORD_ID' :
                iv_zzname  = 'CATE_ABAP' IMPORTING ev_p_value = lv_cate_ap ),
                iv_zzname  = 'CATE_IDOC' IMPORTING ev_p_value = lv_cate_id ),
                iv_zzname  = 'TYPE_INB'  IMPORTING ev_p_value = lv_type_ib ),
                iv_zzname  = 'TYPE_OUB'  IMPORTING ev_p_value = lv_type_ob ).

  IF iv_sysid = space.
    lv_subobj = lv_sysid = sy-sysid.
  ELSE.
    lv_subobj = lv_sysid = iv_sysid.
  ENDIF.

  lv_date     = sy-datum.
  lv_time     = sy-uzeit.
  lt_intf_ack = it_intf_ack.
  CONVERT DATE lv_date TIME lv_time
     INTO TIME STAMP lv_tstmp
          TIME ZONE 'UTC'.

  CLEAR lv_intf_id.
  IF iv_intf_cate = lv_cate_id AND
     iv_intf_type = lv_type_ob AND
     iv_intf_id   = space.
    "Select Number range of object
    SELECT SINGLE nrrangenr
      INTO @DATA(lv_nrrangenr)
      FROM nriv
     WHERE object    EQ 'ZEINTF_DOC'
       AND subobject EQ @lv_subobj.

    IF sy-subrc = 0.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = lv_nrrangenr
          object                  = 'ZEINTF_DOC'
          subobject               = lv_subobj
        IMPORTING
          number                  = lv_intf_id
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        lv_intf_id = |{ lv_sysid }{ lv_intf_id+8(12) }|.
      ENDIF.
    ENDIF.
  ELSE.
    lv_intf_id = iv_intf_id.
  ENDIF.

* Return interface ID. to the system
  ch_intf_id = lv_intf_id.

  IF lv_intf_id IS NOT INITIAL.
    "Populate ack for each individual IDoc key (|<Message Type>|<IDoc key>
    APPEND INITIAL LINE TO lt_intf_ack ASSIGNING FIELD-SYMBOL(<lf_intf_ack>).
    if iv_partner is not initial.
      <lf_intf_ack>-location   = iv_partner.
    else.
      <lf_intf_ack>-location   = 'Generic'.
    endif.
    <lf_intf_ack>-msg_id     = lv_intf_id.
    <lf_intf_ack>-total      = 1.
    <lf_intf_ack>-start_time = lv_tstmp.

*    "Find existing entry (from database + memory)
*    SELECT * FROM ztca_intf_log
*      INTO TABLE lt_intf_log_ref
*      WHERE zid = lv_intf_id
*      ORDER BY PRIMARY KEY.

    LOOP AT lt_intf_ack INTO DATA(lw_intf_ack).
      CLEAR lw_intf_log.
*      READ TABLE lt_intf_log_ref INTO DATA(lw_intf_log_ref) WITH KEY primary_key
*                                 COMPONENTS zid      = lv_intf_id
*                                            zbus_obj = iv_intf_bus_obj.
*      IF sy-subrc = 0.
*        "Found existing entry with same ID and Business object, update only the number
*        lw_intf_log       = lw_intf_log_ref.
*        ADD 1 TO lw_intf_log-ztotal_record.
*      ELSE.
*        "No existing entry, create new
        lw_intf_log       = VALUE ztca_intf_log( zzintfid        = lw_intf_ack-msg_id
                                                 zzbus_obj       = iv_intf_bus_obj
                                                 zzdocnum        = iv_docnum
                                                 zztype          = iv_intf_type
                                                 zzcate          = iv_intf_cate
                                                 zzmodule        = iv_intf_module
                                                 zzreceiver      = iv_intf_buscomp
                                                 zzstore         = lw_intf_ack-location
                                                 zzmessage_type  = iv_mestyp
                                                 zztotal_record  = lw_intf_ack-total
                                                 zzother_ack     = |{ lv_tstmp }000|
                                                 ernam           = sy-uname
                                                 erdat           = lv_date
                                                 erzet           = lv_time ).
*      ENDIF.
      APPEND lw_intf_log TO lt_intf_log.
    ENDLOOP.

    IF lines( lt_intf_log ) GT 0.
      "Update
      MODIFY ztca_intf_log FROM TABLE lt_intf_log.

*      "Collect into memory
*      APPEND LINES OF lt_intf_log TO lt_intf_log_mem.
    ENDIF.
  ELSE.
    RAISE generate_id_error.
  ENDIF.

ENDFUNCTION.
