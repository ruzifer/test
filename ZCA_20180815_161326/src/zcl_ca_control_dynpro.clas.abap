CLASS zcl_ca_control_dynpro DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ca_mvc_control.
    INTERFACES zif_ca_control_dynpro.

    DATA:
      av_program TYPE sy-repid READ-ONLY,
      av_dynpro  TYPE sy-dynnr READ-ONLY.

    ALIASES at_exit_command
      FOR zif_ca_mvc_control~at_exit_command .
    ALIASES call_screen
      FOR zif_ca_mvc_control~call_screen .
    ALIASES pf_status
      FOR zif_ca_mvc_control~pf_status .
    ALIASES process_after_input
      FOR zif_ca_mvc_control~process_after_input .
    ALIASES process_before_output
      FOR zif_ca_mvc_control~process_before_output .
    ALIASES process_on_help_request
      FOR zif_ca_mvc_control~process_on_help_request .
    ALIASES process_on_value_request
      FOR zif_ca_mvc_control~process_on_value_request .
    ALIASES set_dynpro
      FOR zif_ca_mvc_control~set_dynpro .
    ALIASES titlebar
      FOR zif_ca_mvc_control~titlebar .
    ALIASES user_command
      FOR zif_ca_mvc_control~user_command .
    ALIASES transp_itab_in
      FOR zif_ca_mvc_control~transp_itab_in .
    ALIASES transp_itab_out
      FOR zif_ca_mvc_control~transp_itab_out .
    ALIASES alv_top_of_page
      FOR zif_ca_control_dynpro~alv_top_of_page.
    ALIASES alv_pr_top_of_page
      FOR zif_ca_control_dynpro~alv_pr_top_of_page.
    ALIASES alv_top_of_list
      FOR zif_ca_control_dynpro~alv_top_of_list.
    ALIASES alv_subtotal_text
      FOR zif_ca_control_dynpro~alv_subtotal_text.
    ALIASES alv_toolbar_set
      FOR zif_ca_control_dynpro~alv_toolbar_set.
    ALIASES alv_data_changed
      FOR zif_ca_control_dynpro~alv_data_changed.
    ALIASES alv_data_changed_finished
      FOR zif_ca_control_dynpro~alv_data_changed_finished.
    ALIASES alv_double_click
      FOR zif_ca_control_dynpro~alv_double_click.
    ALIASES alv_hotspot_click
      FOR zif_ca_control_dynpro~alv_hotspot_click.
    ALIASES alv_on_f4
      FOR zif_ca_control_dynpro~alv_on_f4.
    ALIASES delayed_change_sel
      FOR zif_ca_control_dynpro~delayed_change_sel.
    CLASS-METHODS get_controller
      IMPORTING
        iv_repid           TYPE sy-repid OPTIONAL
        iv_funcpool        TYPE rs38l_area OPTIONAL
        iv_dynnr           TYPE sy-dynnr OPTIONAL
        iv_name            TYPE string   OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_ca_control_dynpro.
    CLASS-METHODS create_controller
      IMPORTING
        iv_repid           TYPE sy-repid
        iv_funcpool        TYPE rs38l_area OPTIONAL
        iv_dynnr           TYPE sy-dynnr
        iv_name            TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_ca_control_dynpro.
  PROTECTED SECTION.
    CONSTANTS:
      cc_program  TYPE char10 VALUE '\PROGRAM=',
      cc_funcpool TYPE char15 VALUE '\FUNCTION-POOL=',
      cc_class    TYPE char10 VALUE '\CLASS='.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF cty_controller,
        repid    TYPE syrepid,
        dynnr    TYPE sy-dynnr,
        name     TYPE string,
        instance TYPE REF TO zcl_ca_control_dynpro,
      END OF cty_controller,

      ctt_controller TYPE STANDARD TABLE OF cty_controller WITH KEY repid dynnr.

    CLASS-DATA:
      st_controller TYPE ctt_controller.
ENDCLASS.



CLASS zcl_ca_control_dynpro IMPLEMENTATION.
  METHOD zif_ca_mvc_control~set_dynpro.
    av_program = iv_repid.
    av_dynpro  = iv_dynnr.
  ENDMETHOD.


  METHOD get_controller.
    TRY.
        IF iv_repid IS NOT INITIAL AND
           iv_dynnr IS NOT INITIAL.
          ro_instance = st_controller[ repid = iv_repid dynnr = iv_dynnr ]-instance.
        ELSEIF iv_name IS NOT INITIAL.
          ro_instance = st_controller[ name = iv_name ]-instance.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        ro_instance = create_controller( iv_repid    = iv_repid
                                         iv_dynnr    = iv_dynnr
                                         iv_funcpool = iv_funcpool
                                         iv_name     = iv_name ).
    ENDTRY.

    IF ro_instance IS BOUND.
      IF sy-dynnr IS NOT INITIAL AND
         sy-repid IS NOT INITIAL.
        "Set up Program & Dynpro for actual Controller Instance
        ro_instance->set_dynpro( iv_repid = iv_repid iv_dynnr = iv_dynnr ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_controller.
    IF iv_funcpool IS NOT INITIAL.
      DATA(lv_type) = |{ cc_funcpool }{ iv_funcpool }{ cc_class }{ iv_name }|.
    ELSE.
      lv_type = |{ cc_program }{ iv_repid }{ cc_class }{ iv_name }|.
    ENDIF.
    CREATE OBJECT ro_instance TYPE (lv_type)
      EXPORTING
        iv_repid = iv_repid
        iv_dynnr = iv_dynnr.
    st_controller = VALUE #( BASE st_controller ( repid = iv_repid dynnr = iv_dynnr name = iv_name instance = ro_instance ) ).
  ENDMETHOD.

ENDCLASS.
