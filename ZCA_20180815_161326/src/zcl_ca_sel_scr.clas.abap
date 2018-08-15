CLASS zcl_ca_sel_scr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF cty_refflds,
        field  TYPE char8,
        reffld TYPE REF TO data,
      END OF cty_refflds,
      ctt_refflds TYPE STANDARD TABLE OF cty_refflds WITH KEY field.

    TYPES:
      BEGIN OF cty_sel,
        d_sscr     TYPE REF TO sscrfields,
        d_sel      TYPE REF TO data,
        t_seltab   TYPE cvt_seltab,
        t_scr_req  TYPE fieldname_tab,
        t_scr_hide TYPE fieldname_tab,
        t_scr_inp  TYPE fieldname_tab,
        t_scr_outp TYPE fieldname_tab,
      END OF cty_sel.
    DATA:
      aw_sel TYPE cty_sel.
    DATA:
      av_program TYPE sy-repid.
    METHODS init
      CHANGING
        cw_sscr    TYPE sscrfields
        ct_refflds TYPE ctt_refflds OPTIONAL.
    METHODS pai
      IMPORTING
        iv_ucomm TYPE sy-ucomm
        iv_dynnr TYPE sy-dynnr.
    METHODS pbo
      IMPORTING
        iv_dynnr TYPE sy-dynnr.
  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF ccw_selkind,
        param  TYPE rsscr_kind VALUE 'P', "Parameter
        selopt TYPE rsscr_kind VALUE 'S', "Select-option
      END OF ccw_selkind.

    METHODS clr_sel_opt.
    METHODS pbo_1000.
    METHODS modify_screen.
    METHODS parse_selection_screen.
    METHODS validate_input.
    METHODS chk_inp
      IMPORTING
        iv_name       TYPE any
      RETURNING
        VALUE(rv_inp) TYPE numc1.
    METHODS chk_req
      IMPORTING
        iv_name       TYPE any
      RETURNING
        VALUE(rv_req) TYPE numc1.
    METHODS scr_val
      IMPORTING
        iv_fieldname    TYPE fieldname
        iw_struc        TYPE any
      RETURNING
        VALUE(rv_value) TYPE boolean.
    METHODS chk_act
      IMPORTING
        iv_name       TYPE any
      RETURNING
        VALUE(rv_act) TYPE numc1.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_sel_scr IMPLEMENTATION.
  METHOD chk_inp.
    rv_inp = '1'.
    LOOP AT aw_sel-t_scr_inp ASSIGNING FIELD-SYMBOL(<lf_scr_hide>).
      IF iv_name CS <lf_scr_hide>.
        rv_inp = '0'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD chk_act.
    rv_act = '1'.
    LOOP AT aw_sel-t_scr_hide ASSIGNING FIELD-SYMBOL(<lf_scr_hide>).
      IF iv_name CS <lf_scr_hide>.
        rv_act = '0'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD chk_req.
    LOOP AT aw_sel-t_scr_req ASSIGNING FIELD-SYMBOL(<lf_scr_req>)
      WHERE table_line IS NOT INITIAL.
      IF iv_name CS <lf_scr_req>.
        IF iv_name NP '*-HIGH'.
          rv_req = '2'.
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD clr_sel_opt.
    ASSIGN aw_sel-d_sel->* TO FIELD-SYMBOL(<lf_sel>).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <lf_sel> TO FIELD-SYMBOL(<lf_comp>).
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      DESCRIBE FIELD <lf_comp> TYPE DATA(lv_type).
      IF lv_type NE 'l' AND lv_type NE 'r'.
        CLEAR <lf_comp>.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD init.
    aw_sel-d_sscr = REF #( cw_sscr ).
  ENDMETHOD.

  METHOD modify_screen.
    LOOP AT SCREEN.
      screen-required = chk_req( screen-name ).
      screen-active = chk_act( screen-name ).
      IF screen-group3 NE 'OPU'.
        screen-input = chk_inp( screen-name ).
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    parse_selection_screen( ).
    validate_input( ).
  ENDMETHOD.

  METHOD parse_selection_screen.
    FIELD-SYMBOLS: <lf_selopt> TYPE STANDARD TABLE.

    clr_sel_opt( ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = av_program
      TABLES
        selection_table = aw_sel-t_seltab
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.

    CHECK sy-subrc EQ 0.
    LOOP AT aw_sel-t_seltab INTO DATA(lw_seltab) GROUP BY ( selname = lw_seltab-selname )
      ASSIGNING FIELD-SYMBOL(<lf_grpsel>).
      CHECK NOT line_exists( aw_sel-t_scr_hide[ table_line = <lf_grpsel>-selname ] ).

      LOOP AT GROUP <lf_grpsel> ASSIGNING FIELD-SYMBOL(<lf_seltab>).
        DATA(lv_fldnm) = |AW_SEL-D_SEL->{ <lf_seltab>-selname }|.
        ASSIGN (lv_fldnm) TO FIELD-SYMBOL(<lf_comp>).
        CHECK <lf_comp> IS ASSIGNED.
        DESCRIBE FIELD <lf_comp> TYPE DATA(lv_type).
        IF lv_type EQ 'l' OR lv_type EQ 'r'.
          UNASSIGN <lf_comp>.
        ENDIF.
        CHECK <lf_comp> IS ASSIGNED.

        CASE <lf_seltab>-kind.
          WHEN ccw_selkind-param.
            ASSIGN (lv_fldnm) TO FIELD-SYMBOL(<lf_param>).
            CHECK <lf_param> IS ASSIGNED.
          WHEN ccw_selkind-selopt.
            ASSIGN (lv_fldnm) TO <lf_selopt>.
            CHECK <lf_selopt> IS ASSIGNED.
        ENDCASE.

*       Append data
        CASE <lf_seltab>-kind.
          WHEN ccw_selkind-param.
            <lf_param> = <lf_seltab>-low.
            UNASSIGN <lf_param>.
          WHEN ccw_selkind-selopt.
            CHECK <lf_seltab>-sign IS NOT INITIAL.
            APPEND INITIAL LINE TO <lf_selopt> ASSIGNING FIELD-SYMBOL(<lf_append>).
            MOVE-CORRESPONDING <lf_seltab> TO <lf_append>.
            UNASSIGN: <lf_selopt>,
                      <lf_append>.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD pbo_1000.
    REFRESH: aw_sel-t_scr_req,
             aw_sel-t_scr_hide,
             aw_sel-t_scr_inp,
             aw_sel-t_scr_outp.
*    aw_scr-t_scr_req = VALUE #( ( 'S_BADAT' ) ).
*    modify_screen( ).
  ENDMETHOD.

  METHOD scr_val.
    DATA
      ld_value TYPE REF TO data.
    FIELD-SYMBOLS
      <lf_value> TYPE any.

    SPLIT iv_fieldname AT '-' INTO DATA(lv_fldnm) DATA(lv_temp).

    DATA(lv_offset) = strlen( lv_fldnm ).
    DESCRIBE FIELD iw_struc TYPE DATA(lv_type).
    CASE lv_type.
      WHEN 'l'. "Object
        DATA(lv_comp) = |IW_STRUC->{ lv_fldnm(lv_offset) }|.
        ASSIGN (lv_comp) TO FIELD-SYMBOL(<lf_comp>).
      WHEN OTHERS.
        ASSIGN COMPONENT lv_fldnm(lv_offset) OF STRUCTURE iw_struc TO <lf_comp>.
    ENDCASE.
    IF <lf_comp> IS ASSIGNED.
      DESCRIBE FIELD <lf_comp> TYPE lv_type.
      CASE lv_type.
        WHEN 'l'. "Object
          ld_value = CAST #( <lf_comp> ).
          ASSIGN ld_value->* TO <lf_value>.
          IF <lf_value> IS NOT INITIAL.
            rv_value = abap_true.
          ENDIF.
        WHEN OTHERS.
          IF <lf_comp> IS NOT INITIAL.
            rv_value = abap_true.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD validate_input.
    IF aw_sel-d_sscr->ucomm EQ 'ONLI'. "Execute
      ASSIGN aw_sel-d_sel->* TO FIELD-SYMBOL(<lf_sel>).
      LOOP AT aw_sel-t_scr_req  ASSIGNING FIELD-SYMBOL(<lf_scr_req>).
        IF scr_val( iv_fieldname = <lf_scr_req>
                    iw_struc     = <lf_sel> ) EQ abap_false.
          SET CURSOR FIELD <lf_scr_req>.
          MESSAGE e055(00).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD pbo.
    DATA(lv_method) = |PBO_{ iv_dynnr }|.
    TRY.
        CALL METHOD (lv_method).
      CATCH cx_sy_dyn_call_error.
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
