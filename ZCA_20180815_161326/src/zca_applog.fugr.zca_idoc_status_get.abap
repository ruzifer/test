FUNCTION zca_idoc_status_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DOCNUM) TYPE  EDI_DOCNUM
*"  TABLES
*"      TT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
*  "Assuming it is error!
*  ev_error = abap_true.

  "Get IDoc status
  SELECT * FROM edids INTO TABLE @DATA(lt_edids)
    WHERE docnum = @iv_docnum.
  CHECK sy-subrc = 0.

  LOOP AT lt_edids INTO DATA(lw_edids).
    IF     lw_edids-statyp IS NOT INITIAL
       AND lw_edids-stamid IS NOT INITIAL
       AND lw_edids-stamno IS NOT INITIAL.
      APPEND INITIAL LINE TO tt_return ASSIGNING FIELD-SYMBOL(<lf_return>).
      <lf_return>-type        = lw_edids-statyp.
      <lf_return>-id          = lw_edids-stamid.
      <lf_return>-number      = lw_edids-stamno.
      <lf_return>-message_v1  = lw_edids-stapa1.
      <lf_return>-message_v2  = lw_edids-stapa2.
      <lf_return>-message_v3  = lw_edids-stapa3.
      <lf_return>-message_v4  = lw_edids-stapa4.
      <lf_return>-system      = |{ sy-sysid }_{ sy-mandt }|.
      <lf_return>-parameter   = lw_edids-status.
      MESSAGE ID lw_edids-stamid
            TYPE lw_edids-statyp
            NUMBER lw_edids-stamno
            WITH lw_edids-stapa1 lw_edids-stapa2 lw_edids-stapa3 lw_edids-stapa4
            INTO <lf_return>-message.
    ENDIF.

    IF lw_edids-appl_log IS NOT INITIAL.
      "With application log...
      DATA: lt_return TYPE bapiret2_t,
            lw_return TYPE bapiret2.

      CLEAR lt_return.
      CALL FUNCTION 'ZCA_APP_LOG_WITH_LOGNO'
        EXPORTING
          iv_lognumber = lw_edids-appl_log
        TABLES
          tt_return    = lt_return.
      CHECK lines( lt_return ) GT 0.

      lw_return-parameter = lw_edids-status.
      MODIFY lt_return FROM lw_return TRANSPORTING parameter
        WHERE parameter IS INITIAL.

      APPEND LINES OF lt_return TO tt_return.
    ENDIF.
  ENDLOOP.

*  ev_error = abap_false.
*  LOOP AT tt_return TRANSPORTING NO FIELDS
*                    WHERE type CA 'AEX'.
*    ev_error = abap_true.
*  ENDLOOP.
ENDFUNCTION.
