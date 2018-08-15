*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

**&---------------------------------------------------------------------*
**&      Form  INIT_VALUES
**&---------------------------------------------------------------------*
*  FORM init_values.
*    IF gr_langu_e[] IS INITIAL.
*      zclca_util=>read_variable_value(
*                         EXPORTING im_repid = 'SAPLZTAPP_TCUR_WORD'
*                                   im_name  = 'LANGUAGE_ENGLISH'
*                         IMPORTING ex_values = gr_langu_e[] ).
*    ENDIF.
*  ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SPELL_CHINESE
**&---------------------------------------------------------------------*
*  FORM spell_chinese  USING p_amount
*                   CHANGING ps_in_words TYPE zsca_spell.
*
*    DATA: lv_amount TYPE fiappt_amount. "BSEG-DMBTR.
*    lv_amount = p_amount.
*
*    CLEAR ps_in_words-word_full.
*    IF lv_amount = 0.
*      ps_in_words-word_full = '零'.
*      EXIT.
*    ENDIF.
*
*    DATA: money_str(33).
*    money_str = lv_amount.
*    CONDENSE money_str NO-GAPS.
*    IF money_str CN '0123456789. '.
*      RAISE wrong_money.
*    ENDIF.
*
*    DATA: i TYPE i.
*    IF money_str CS '.'.
*      i = sy-fdpos + 1.
*      money_str+sy-fdpos = money_str+i.
*    ENDIF.
*    CONDENSE money_str NO-GAPS.
*
*    DATA:units_off TYPE i,
*         curnt_off TYPE i.
*    DATA:lastd  TYPE n,curntd TYPE n.
*    DATA:cword(2),weight(2).
*    DATA:units(30) VALUE '分角圆拾佰仟萬拾佰仟亿拾佰仟万',
*         digts(20) VALUE '零壹贰叁肆伍陆柒捌玖'.
** clear:ps_in_words-word_full,units_off.
*    lastd = 0.
*    curnt_off = strlen( money_str ) - 1.
*    WHILE curnt_off >= 0.
*      curntd = money_str+curnt_off(1).
*      i      = curntd.
*      cword  = digts+i(1).
*      weight = units+units_off(1).
*      i      = units_off / 1.
*      IF curntd = 0.             "Current digit is 0
*        IF i = 2 OR i = 6 OR i = 10.
*          CLEAR: cword.
*          IF curnt_off = 0.
*            CLEAR: weight.
*          ENDIF.
*        ELSEIF lastd = 0.
*          CLEAR: cword,weight.
*        ELSE.
*          CLEAR: weight.
*        ENDIF.
*      ENDIF.
*      CONCATENATE cword weight ps_in_words-word_full
*        INTO ps_in_words-word_full.
*      lastd = curntd.
*      SUBTRACT 1 FROM curnt_off.
*      ADD 1 TO units_off.
*    ENDWHILE.
*    IF ps_in_words-word_full NS '分'.
*      CONCATENATE ps_in_words-word_full '整' INTO ps_in_words-word_full.
*    ELSE.
*      cword = ps_in_words-word_full.
*      IF cword = '零'.
*        SHIFT ps_in_words-word_full BY 1 PLACES.
*      ENDIF.
*    ENDIF.
*  ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  FIRSTCHAR_UPPER
**&---------------------------------------------------------------------*
*  FORM firstchar_upper  CHANGING p_text.
*    CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
*      EXPORTING
*        input_string  = p_text
*      IMPORTING
*        output_string = p_text.
*
*  ENDFORM.
