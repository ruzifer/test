FUNCTION ZCA_APP_LOG_CREATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_OBJECT) TYPE  BALOBJ_D
*"     VALUE(IV_SUBOBJECT) TYPE  BALSUBOBJ
*"     VALUE(IV_EXT_NO) TYPE  BALNREXT OPTIONAL
*"     VALUE(IT_TEXT) TYPE  ZTTCA_MESSAGE OPTIONAL
*"  EXPORTING
*"     VALUE(EX_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  "Craete Log
  DATA(lv_handle) = zcl_ca_utility=>log_create( iv_object    = iv_object
                                                iv_subobject = iv_subobject
                                                iv_ext_no    = iv_ext_no ).
  "Add text to Application log
  IF it_text IS NOT INITIAL.
    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<lfs_text>).
      zcl_ca_utility=>log_add_message_free_text( iv_handle = lv_handle
                                                 iv_msgty  = <lfs_text>-msgty
                                                 iv_text   = <lfs_text>-text ).
    ENDLOOP.
  ENDIF.
  "Save log
  DATA(lv_logno) = zcl_ca_utility=>log_save( iv_loghandle = lv_handle ).
  "Return status
  IF lv_logno IS INITIAL.
    DATA(ls_return) = VALUE bapiret2( type = 'E' id = 'ZCA00' number = '079' ).
  ELSE.
    ls_return = VALUE bapiret2( type = 'E' id = 'ZCA00' number = '078' message_v1 = CONV #( lv_logno ) ).
  ENDIF.
  MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
     WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
     INTO ls_return-message.
  ex_return = VALUE #( ( ls_return ) ).
ENDFUNCTION.
