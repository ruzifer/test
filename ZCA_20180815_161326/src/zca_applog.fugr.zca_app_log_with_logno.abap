FUNCTION zca_app_log_with_logno.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LOGNUMBER) TYPE  BALHDR-LOGNUMBER
*"  TABLES
*"      TT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  CLEAR tt_return.

  CHECK iv_lognumber IS NOT INITIAL.

  DATA: lt_messages TYPE tab_balm_nokey_sc.
  DATA(lt_lognumber) = VALUE szal_lognumbers( ( item = iv_lognumber ) ).

  CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
    TABLES
      lognumbers = lt_lognumber
      messages   = lt_messages.

  CHECK lines( lt_messages ) GT 0.

  LOOP AT lt_messages INTO DATA(lw_msg).
    APPEND INITIAL LINE TO tt_return ASSIGNING FIELD-SYMBOL(<lf_return>).
    <lf_return>-type        = lw_msg-msgty.
    <lf_return>-id          = lw_msg-msgid.
    <lf_return>-number      = lw_msg-msgno.
    <lf_return>-message_v1  = lw_msg-msgv1.
    <lf_return>-message_v2  = lw_msg-msgv2.
    <lf_return>-message_v3  = lw_msg-msgv3.
    <lf_return>-message_v4  = lw_msg-msgv4.
    <lf_return>-row         = lw_msg-msgnumber.
    <lf_return>-system      = |{ sy-sysid }_{ sy-mandt }|.
    MESSAGE ID lw_msg-msgid
          TYPE lw_msg-msgty
          NUMBER lw_msg-msgno
          WITH lw_msg-msgv1 lw_msg-msgv2 lw_msg-msgv3 lw_msg-msgv4
          INTO <lf_return>-message.
    <lf_return>-log_no      = iv_lognumber.
*    <lf_return>-LOG_MSG_NO
*    <lf_return>-PARAMETER
*    <lf_return>-FIELD
  ENDLOOP.

ENDFUNCTION.
