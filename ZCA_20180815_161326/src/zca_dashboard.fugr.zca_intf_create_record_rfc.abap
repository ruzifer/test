FUNCTION zca_intf_create_record_rfc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SYSID) TYPE  SYST_SYSID OPTIONAL
*"  TABLES
*"      IT_IDOC_DATA STRUCTURE  EDIDD
*"  CHANGING
*"     VALUE(CH_IDOC_CONTROL) LIKE  EDIDC STRUCTURE  EDIDC OPTIONAL
*"     VALUE(CH_INTF_ID) TYPE  ZCA_INTF_GUID OPTIONAL
*"     VALUE(CH_PARTNER) TYPE  EDIPPARNUM OPTIONAL
*"  EXCEPTIONS
*"      IDOC_INPUT_INCONSISTENT
*"----------------------------------------------------------------------

  DATA: lv_flg_err TYPE flag.

  STATICS: st_intf_map TYPE zttca_intf_map.

  IF lines( st_intf_map ) EQ 0.
    SELECT * FROM ztca_intf_map
      INTO TABLE st_intf_map ORDER BY PRIMARY KEY.
  ENDIF.

  DATA(lt_intf_map) = VALUE zttca_intf_map( FOR lw_map IN st_intf_map
                                            WHERE ( mestyp = ch_idoc_control-mestyp
                                              AND   idoctp = ch_idoc_control-idoctp
                                              AND   cimtyp = ch_idoc_control-cimtyp
                                              AND   mesfct = ch_idoc_control-mesfct )
                                              ( lw_map ) ).
  IF lines( lt_intf_map ) GT 0.
    IF ch_idoc_control-rcvprt = 'KU'.
      ch_partner = ch_idoc_control-rcvprn.
    ENDIF.

    CLEAR: lv_flg_err, ch_intf_id.
    "Loop through all receiver components found for this interface
    LOOP AT lt_intf_map INTO DATA(lw_intf_map)
                       WHERE zzmodule IS NOT INITIAL.
      "Generate the number only once
      CALL FUNCTION 'ZCA_INTF_CREATE_RECORD_ID'
        EXPORTING
          iv_intf_id        = ch_intf_id                 "Same
          iv_intf_type      = lw_intf_map-zztype         "Same
          iv_intf_cate      = lw_intf_map-zzcate         "Same
          iv_intf_module    = lw_intf_map-zzmodule       "Same
          iv_intf_bus_obj   = lw_intf_map-zzbus_obj      "Difference
          iv_intf_buscomp   = lw_intf_map-businesscomp   "Difference
          iv_mestyp         = ch_idoc_control-mestyp     "Same
          iv_partner        = ch_partner                 "Same
          iv_sysid          = iv_sysid                   "System ID.
        CHANGING
          ch_intf_id        = ch_intf_id
        EXCEPTIONS
          generate_id_error = 1
          OTHERS            = 2.
      IF sy-subrc NE 0.
        lv_flg_err = abap_true.
        "Unexpected error -> exit from loop.
        EXIT.
      ENDIF.
    ENDLOOP.
    ch_idoc_control-sndlad = ch_intf_id.

    IF lv_flg_err = abap_true.
      RAISE idoc_input_inconsistent.
    ENDIF.
  ENDIF.

ENDFUNCTION.
