*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE ADD_PARAM.
  CLEAR lw_param.
  lw_param-name = &3.
  CASE &2.
    WHEN cc_type-export.
      lw_param-kind = cl_abap_objectdescr=>exporting.
    WHEN cc_type-import.
      lw_param-kind = cl_abap_objectdescr=>importing.
  ENDCASE.
  GET REFERENCE OF &1 INTO lw_param-value.
  INSERT lw_param INTO TABLE lt_param.
END-OF-DEFINITION.
DEFINE ADD_PARAM_OPT.
  IF &1 IS SUPPLIED.
    CLEAR lw_param.
    lw_param-name = &3.
    CASE &2.
      WHEN cc_type-export.
        lw_param-kind = cl_abap_objectdescr=>exporting.
      WHEN cc_type-import.
        lw_param-kind = cl_abap_objectdescr=>importing.
    ENDCASE.
    GET REFERENCE OF &1 INTO lw_param-value.
    INSERT lw_param INTO TABLE lt_param.
  ENDIF.
END-OF-DEFINITION.
DEFINE CONCAT_ADDR.
  CONCATENATE &1 &2 INTO &1 SEPARATED BY space.
END-OF-DEFINITION.
DEFINE CONCAT_ADDR_WIDTH.
  CONCATENATE &1 lv_num INTO lv_fnam.
  UNASSIGN <fs_field>.
  ASSIGN (lv_fnam) TO <fs_field>.

  IF <fs_field> IS ASSIGNED.
    IF &2 IS NOT INITIAL AND &2 NE space.
      IF <fs_field> = space.
        <fs_field> = &2.
      ELSE.
        CONCATENATE <fs_field> &2 INTO lv_string SEPARATED BY space.

        CLEAR lv_length.
        CALL METHOD get_string_width
          EXPORTING
            iv_devtype     = iv_devtype
            iv_sapfont     = iv_sapfont
            iv_fontsize    = iv_fontsize
            iv_bold        = iv_bold
            iv_italic      = iv_italic
            iv_string      = lv_string
            iv_length_unit = iv_width_unit
          IMPORTING
            ev_length      = lv_length
          EXCEPTIONS
            ex_error       = 1
            OTHERS         = 2.
        IF sy-subrc = 0.
          IF lv_length LT iv_width.   "Space is still enough
            <fs_field> = lv_string.
          ELSE.                      "Space is not enough, move to new line
            ADD 1 TO lv_num.
            CONCATENATE &1 lv_num INTO lv_fnam.
            UNASSIGN <fs_field>. ASSIGN (lv_fnam) TO <fs_field>.
            IF <fs_field> IS ASSIGNED.
              IF &2 IS NOT INITIAL AND &2 NE space.
                <fs_field> = &2.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.
DEFINE CONVERSION_INPUT.
  CALL FUNCTION &1
    EXPORTING
      input  = &2
    IMPORTING
      output = &3
    EXCEPTIONS
      OTHERS = 8.
END-OF-DEFINITION.
DEFINE FIRSTCHAR_UPPER.
  CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
    EXPORTING
      input_string  = &1
    IMPORTING
      output_string = &1.
END-OF-DEFINITION.
