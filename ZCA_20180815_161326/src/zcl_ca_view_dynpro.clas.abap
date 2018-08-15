CLASS zcl_ca_view_dynpro DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ca_mvc_view.
    INTERFACES zif_ca_view_dynpro.
    DATA:
      av_program TYPE sy-repid READ-ONLY,
      av_dynpro  TYPE sy-dynnr READ-ONLY.
    ALIASES trigger_at_exit_command
      FOR zif_ca_mvc_view~trigger_at_exit_command .
*    ALIASES trigger_on_link_click
*      FOR zif_ca_mvc_view~trigger_on_link_click .
*    ALIASES trigger_on_double_click
*      FOR zif_ca_mvc_view~trigger_on_double_click .
    ALIASES trigger_pai
      FOR zif_ca_mvc_view~trigger_pai .
    ALIASES trigger_pbo
      FOR zif_ca_mvc_view~trigger_pbo .
    ALIASES trigger_poh
      FOR zif_ca_mvc_view~trigger_poh .
    ALIASES trigger_pov
      FOR zif_ca_mvc_view~trigger_pov .
    ALIASES trigger_status
      FOR zif_ca_mvc_view~trigger_status .
    ALIASES trigger_titlebar
      FOR zif_ca_mvc_view~trigger_titlebar .
    ALIASES trigger_user_command
      FOR zif_ca_mvc_view~trigger_user_command .
    ALIASES trigger_itab_in
      FOR zif_ca_mvc_view~trigger_itab_in.
    ALIASES trigger_itab_out
      FOR zif_ca_mvc_view~trigger_itab_out.
    ALIASES at_exit_command
      FOR zif_ca_mvc_view~at_exit_command .
    ALIASES transp_itab_in
      FOR zif_ca_mvc_view~transp_itab_in .
    ALIASES transp_itab_out
      FOR zif_ca_mvc_view~transp_itab_out .
    ALIASES pf_status
      FOR zif_ca_mvc_view~pf_status .
    ALIASES process_after_input
      FOR zif_ca_mvc_view~process_after_input .
    ALIASES process_before_output
      FOR zif_ca_mvc_view~process_before_output .
    ALIASES process_on_help_request
      FOR zif_ca_mvc_view~process_on_help_request .
    ALIASES process_on_value_request
      FOR zif_ca_mvc_view~process_on_value_request .
    ALIASES titlebar
      FOR zif_ca_mvc_view~titlebar .
    ALIASES user_command
      FOR zif_ca_mvc_view~user_command .
    ALIASES set_dynpro
      FOR zif_ca_mvc_view~set_dynpro .

    ALIASES alv_on_tree_drag
      FOR zif_ca_view_dynpro~alv_on_tree_drag.
    ALIASES alv_on_tree_drop
      FOR zif_ca_view_dynpro~alv_on_tree_drop.
    ALIASES alv_toolbar_set
      FOR zif_ca_view_dynpro~alv_toolbar_set.
    ALIASES alv_user_command
      FOR zif_ca_view_dynpro~alv_user_command.
    ALIASES alv_hotspot_click
      FOR zif_ca_view_dynpro~alv_hotspot_click .
    ALIASES alv_double_click
      FOR zif_ca_view_dynpro~alv_double_click .
    ALIASES alv_data_changed
      FOR zif_ca_view_dynpro~alv_data_changed.
    ALIASES alv_data_changed_finished
      FOR zif_ca_view_dynpro~alv_data_changed_finished.
    ALIASES delayed_change_sel
      FOR zif_ca_view_dynpro~delayed_change_sel.
    ALIASES alv_on_f4
      FOR zif_ca_view_dynpro~alv_on_f4.
    ALIASES alv_top_of_page
      FOR zif_ca_view_dynpro~alv_top_of_page.
    ALIASES alv_pr_top_of_page
      FOR zif_ca_view_dynpro~alv_pr_top_of_page.
    ALIASES alv_top_of_list
      FOR zif_ca_view_dynpro~alv_top_of_list.
    ALIASES alv_end_of_list
      FOR zif_ca_view_dynpro~alv_end_of_list.
    ALIASES alv_subtotal_text
      FOR zif_ca_view_dynpro~alv_subtotal_text.
    ALIASES trigger_handle_toolbar_set
      FOR zif_ca_view_dynpro~trigger_handle_toolbar_set.
    ALIASES trigger_handle_user_command
      FOR zif_ca_view_dynpro~trigger_handle_user_command.
    ALIASES trigger_handle_data_changed
      FOR zif_ca_view_dynpro~trigger_handle_data_changed.
    ALIASES trigger_handle_data_chg_finish
      FOR zif_ca_view_dynpro~trigger_handle_data_chg_finish.
    ALIASES trigger_handle_on_f4
      FOR zif_ca_view_dynpro~trigger_handle_on_f4.
    ALIASES trigger_handle_double_click
      FOR zif_ca_view_dynpro~trigger_handle_double_click.
    ALIASES trigger_handle_hotspot_click
      FOR zif_ca_view_dynpro~trigger_handle_hotspot_click.
    ALIASES trigger_delayed_change_sel
      FOR zif_ca_view_dynpro~trigger_delayed_change_sel.
    ALIASES trigger_handle_top_of_page
      FOR zif_ca_view_dynpro~trigger_handle_top_of_page.
    ALIASES trigger_handle_top_of_list
      FOR zif_ca_view_dynpro~trigger_handle_top_of_list.
    ALIASES trigger_handle_pr_top_of_page
      FOR zif_ca_view_dynpro~trigger_handle_pr_top_of_page.
    ALIASES trigger_handle_end_of_list
      FOR zif_ca_view_dynpro~trigger_handle_end_of_list.
    ALIASES trigger_handle_subtotal_text
      FOR zif_ca_view_dynpro~trigger_handle_subtotal_text.
    ALIASES edit_alv
      FOR zif_ca_view_dynpro~edit_alv.
    ALIASES refresh_alv
      FOR zif_ca_view_dynpro~refresh_alv.
    ALIASES input_check
      FOR zif_ca_view_dynpro~input_check.
    ALIASES html_display
      FOR zif_ca_view_dynpro~html_display.

    METHODS constructor
      IMPORTING
        !iv_repid               TYPE sy-repid
        !iv_dynnr               TYPE sy-dynnr
        !io_controller_instance TYPE REF TO zif_ca_mvc_control.
  PROTECTED SECTION.
    DATA:
      ao_alv         TYPE REF TO cl_gui_alv_grid,
      ao_container   TYPE REF TO cl_gui_custom_container,
      ao_cusctrl     TYPE REF TO cl_gui_container,
      ao_dyndoc_id   TYPE REF TO cl_dd_document,
      ao_splitter    TYPE REF TO cl_gui_splitter_container,
      ao_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      ao_parent_html TYPE REF TO cl_gui_container,
      ao_alv_tree    TYPE REF TO cl_gui_alv_tree.
    DATA: at_fcat TYPE lvc_t_fcat.
    METHODS:
      build_fcat
        IMPORTING VALUE(it_data) TYPE table.
    METHODS
      exclude_tb_edit_functions
        RETURNING VALUE(rt_exclude) TYPE ui_functions.
    METHODS
      alv_check_box
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_desc
        IMPORTING VALUE(iv_column) TYPE lvc_fname
                  VALUE(iv_text)   TYPE clike.
    METHODS
      alv_column_invisible
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_noout
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_sum
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_no_zero
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_key
        IMPORTING VALUE(iv_column) TYPE lvc_fname.
    METHODS
      sort_alv
        RETURNING VALUE(rt_sort) TYPE lvc_t_sort.
    METHODS
      alv_column_onf4
        IMPORTING
          VALUE(iv_column) TYPE lvc_fname.
    METHODS
      alv_column_hotspot
        IMPORTING
          VALUE(iv_column) TYPE lvc_fname.
    METHODS set_layout
      IMPORTING
        iv_excp_fname    TYPE clike    OPTIONAL
        iv_no_rowmark    TYPE lvc_rowmk OPTIONAL
      RETURNING
        VALUE(rw_layout) TYPE lvc_s_layo.
    METHODS alv_column.
    METHODS set_alv_editable.
    METHODS alv_variant
      RETURNING
        VALUE(rw_variant) TYPE disvariant.
    METHODS register_for_f4.
    METHODS set_alv_handle.
    METHODS set_row_height
      IMPORTING
        io_splitter TYPE REF TO cl_gui_splitter_container.
  PRIVATE SECTION.
    METHODS set_delayed_selection.
    CONSTANTS:
      cc_view_class   TYPE string VALUE 'ZCL_CA_VIEW_DYNPRO', "#EC NOTEXT
      cc_program_name TYPE char9 VALUE '\PROGRAM=',         "#EC NOTEXT
      cc_class_name   TYPE char7 VALUE '\CLASS='.           "#EC NOTEXT
ENDCLASS.



CLASS zcl_ca_view_dynpro IMPLEMENTATION.

  METHOD constructor.

*    me->av_program  = iv_repid.
*    me->av_dynpro   = iv_dynnr.
    set_dynpro( iv_repid = iv_repid
                iv_dynnr = iv_dynnr ).
    "// Register all Event Handlers for View Events
    SET HANDLER io_controller_instance->user_command
                io_controller_instance->process_after_input
                io_controller_instance->process_before_output
                io_controller_instance->pf_status
                io_controller_instance->titlebar
                io_controller_instance->process_on_value_request
                io_controller_instance->process_on_help_request
                io_controller_instance->at_exit_command
                io_controller_instance->transp_itab_in
                io_controller_instance->transp_itab_out FOR me.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~set_dynpro.

    av_program = iv_repid.
    av_dynpro  = iv_dynnr.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_at_exit_command.

    RAISE EVENT at_exit_command EXPORTING ev_ucomm = iv_ucomm
                                          ev_repid = av_program
                                          ev_dynnr = av_dynpro.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_itab_in.

    RAISE EVENT transp_itab_in EXPORTING ev_index = iv_index.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_itab_out.

    RAISE EVENT transp_itab_out EXPORTING ev_index = iv_index.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_pai.

    RAISE EVENT process_after_input EXPORTING ev_repid = av_program
                                              ev_dynnr = av_dynpro.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_pbo.

    RAISE EVENT process_before_output EXPORTING ev_repid = av_program
                                                ev_dynnr = av_dynpro.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_poh.

    RAISE EVENT process_on_help_request EXPORTING ev_field = iv_field.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_pov.

    RAISE EVENT process_on_value_request EXPORTING ev_field = iv_field.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_status.

    RAISE EVENT pf_status EXPORTING ev_repid = av_program
                                    ev_dynnr = av_dynpro.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_titlebar.

    RAISE EVENT titlebar EXPORTING ev_repid = av_program
                                   ev_dynnr = av_dynpro.

  ENDMETHOD.


  METHOD zif_ca_mvc_view~trigger_user_command.

    RAISE EVENT user_command EXPORTING ev_ucomm =  iv_ucomm.

  ENDMETHOD.


  METHOD alv_check_box.
    TRY.
        at_fcat[ fieldname = iv_column ]-checkbox  = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column.
*  The implementation must be made in subclass method if necessary !

  ENDMETHOD.


  METHOD alv_column_desc.
    TRY.
        DATA(ld_fcat) = REF #( at_fcat[ fieldname = iv_column ] ).
        ld_fcat->coltext   = iv_text.
        ld_fcat->scrtext_l = iv_text.
        ld_fcat->reptext   = iv_text.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_hotspot.
    TRY.
        at_fcat[ fieldname = iv_column ]-hotspot = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_key.
    TRY.
        at_fcat[ fieldname = iv_column ]-key = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_no_zero.
    TRY.
        at_fcat[ fieldname = iv_column ]-no_zero = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_onf4.
    TRY.
        at_fcat[ fieldname = iv_column ]-f4availabl = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_sum.
    TRY.
        at_fcat[ fieldname = iv_column ]-do_sum  = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_invisible.
    TRY.
        at_fcat[ fieldname = iv_column ]-tech  = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD alv_column_noout.
    TRY.
        at_fcat[ fieldname = iv_column ]-no_out  = abap_true.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD alv_variant.
    rw_variant = VALUE disvariant( report = sy-cprog ).
  ENDMETHOD.


  METHOD build_fcat.
*...New ALV Instance ...............................................
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = DATA(lo_salv_table)
          CHANGING
            t_table      = it_data ).
      CATCH cx_salv_msg.
    ENDTRY.
    DATA(lo_columns)  = lo_salv_table->get_columns( ).
    DATA(lo_aggregations) = lo_salv_table->get_aggregations( ).

    CALL METHOD cl_salv_controller_metadata=>get_lvc_fieldcatalog
      EXPORTING
        r_columns      = lo_columns
        r_aggregations = lo_aggregations
      RECEIVING
        t_fieldcatalog = at_fcat.
  ENDMETHOD.


  METHOD edit_alv.

    IF ao_cusctrl IS INITIAL.
      IF iv_top_of_page EQ abap_true.
        ao_container = NEW cl_gui_custom_container( iv_cusctrl_nm ).
*       Create TOP-Document
        ao_dyndoc_id = NEW #( style = 'ALV_GRID' ).
        ao_splitter = NEW #( parent  = ao_container
                             rows    = 2
                             columns = 1 ).
        ao_parent_html = ao_splitter->get_container( row    = 1
                                                     column = 1 ).
        ao_cusctrl = ao_splitter->get_container( row    = 2
                                                 column = 1 ).
        set_row_height( ao_splitter ).

      ELSE.
        ao_cusctrl = NEW cl_gui_custom_container( iv_cusctrl_nm ).
      ENDIF.
      ao_alv = NEW cl_gui_alv_grid( ao_cusctrl ).
      "Exclude function
      DATA(lt_exclude) = exclude_tb_edit_functions( ).
      "Set layout
      DATA(lw_layout) = set_layout( ).
      "Build field catalog
      build_fcat( ct_data ).
      alv_column( ).
      "Sort ALV
      DATA(lt_sort) = sort_alv( ).
      "Layout variant
      DATA(lw_variant) = alv_variant( ).
      "Set AL handler
      set_alv_handle( ).
      "ALV Display
      ao_alv->set_table_for_first_display(
        EXPORTING
          is_layout            = lw_layout
          it_toolbar_excluding = lt_exclude
          i_save               = 'A'
          is_variant           = lw_variant
        CHANGING
          it_fieldcatalog      = at_fcat
          it_outtab            = ct_data
          it_sort              = lt_sort ).
      "Set delayed selection change
      set_delayed_selection( ).
      "Set ALV editable
      set_alv_editable( ).
      "Register field for F4
      register_for_f4( ).
      IF iv_top_of_page EQ abap_true.
        "Initializing document
        ao_dyndoc_id->initialize_document( ).
        "Processing events
        ao_alv->list_processing_events( i_event_name = 'TOP_OF_PAGE'
                                        i_dyndoc_id  = ao_dyndoc_id ).
      ENDIF.
    ELSE.
*      refresh_alv( ).
    ENDIF.
  ENDMETHOD.


  METHOD exclude_tb_edit_functions.
* Only allow to change data not to create new entries (exclude
* generic functions).
    rt_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_undo )
                          ( cl_gui_alv_grid=>mc_fc_loc_cut )
                          ( cl_gui_alv_grid=>mc_fc_loc_copy )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                          ( cl_gui_alv_grid=>mc_fc_loc_paste ) ).

  ENDMETHOD.


  METHOD register_for_f4.
    "Example
    "io_alv->register_f4_for_fields( it_f4 = VALUE lvc_t_f4( ( fieldname = 'XXXX' register = abap_true chngeafter = abap_true ) ) ).
  ENDMETHOD.


  METHOD set_alv_editable.
    "set editable cells to ready for input
    ao_alv->set_ready_for_input(
      EXPORTING
        i_ready_for_input = 1 ).

    ao_alv->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

    ao_alv->register_edit_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  ENDMETHOD.


  METHOD set_alv_handle.
    SET HANDLER trigger_handle_data_changed trigger_handle_double_click FOR ao_alv.
  ENDMETHOD.


  METHOD set_layout.
    rw_layout = VALUE lvc_s_layo( cwidth_opt = abap_true ).
  ENDMETHOD.


  METHOD sort_alv.
    "EXAMPLE
    "DATA(lv_spos) = 1.
    "rt_sort = value #( ( spos = lv_spos fieldname = 'XXXXX' up = 'X' ) ).
  ENDMETHOD.


  METHOD trigger_handle_data_changed.
    RAISE EVENT alv_data_changed
      EXPORTING
        ed_data_changed = er_data_changed
        ev_onf4         = e_onf4
        ev_onf4_before  = e_onf4_before
        ev_onf4_after   = e_onf4_after
        ev_ucomm        = e_ucomm.
  ENDMETHOD.


  METHOD trigger_handle_data_chg_finish.
    RAISE EVENT alv_data_changed_finished
      EXPORTING
        et_good_cells = et_good_cells
        ev_modified   = e_modified.
  ENDMETHOD.


  METHOD trigger_handle_double_click.
    RAISE EVENT alv_double_click
      EXPORTING
        ew_row      = e_row
        ew_column   = e_column
        ew_row_no   = es_row_no.
  ENDMETHOD.

  METHOD trigger_handle_hotspot_click.
    RAISE EVENT alv_hotspot_click
      EXPORTING
        ew_row      = e_row_id
        ew_column   = e_column_id
        ew_row_no   = es_row_no.
  ENDMETHOD.


  METHOD trigger_handle_on_f4.
    RAISE EVENT alv_on_f4
      EXPORTING
        ev_fieldname   = e_fieldname
        ev_fieldvalue  = e_fieldvalue
        ew_row_no      = es_row_no
        ed_event_data  = er_event_data
        et_bad_cells   = et_bad_cells
        ev_display     = e_display.
  ENDMETHOD.


  METHOD trigger_handle_toolbar_set.
    CHECK e_object IS BOUND.
    RAISE EVENT alv_toolbar_set
      EXPORTING
        eo_object      = e_object
        ev_interactive = e_interactive.
  ENDMETHOD.


  METHOD trigger_handle_user_command.
    RAISE EVENT user_command EXPORTING ev_ucomm = e_ucomm.
  ENDMETHOD.


  METHOD zif_ca_view_dynpro~refresh_alv.
    DATA(ls_stable) = VALUE  lvc_s_stbl( row = abap_true col = abap_true ).
    ao_alv->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable    " With Stable Rows/Columns
*        i_soft_refresh =     " Without Sort, Filter, etc.
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD input_check.
    ao_alv->check_changed_data(
      IMPORTING
        e_valid = rv_valid ).
  ENDMETHOD.

  METHOD zif_ca_view_dynpro~trigger_delayed_change_sel.
    RAISE EVENT delayed_change_sel.
  ENDMETHOD.

  METHOD set_delayed_selection.
    ao_alv->register_delayed_event(
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select    " Event ID
    ).
    IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD trigger_handle_subtotal_text.
    RAISE EVENT alv_subtotal_text
      EXPORTING
        ew_subtottxt_info  = es_subtottxt_info
        ed_subtot_line     = ep_subtot_line
        eo_event_data      = e_event_data.
  ENDMETHOD.

  METHOD trigger_handle_top_of_page.
    RAISE EVENT alv_top_of_page
      EXPORTING
        eo_dyndoc_id = e_dyndoc_id.
  ENDMETHOD.

  METHOD trigger_handle_pr_top_of_page.
    RAISE EVENT alv_pr_top_of_page.
  ENDMETHOD.

  METHOD trigger_handle_top_of_list.
    RAISE EVENT alv_top_of_list.
  ENDMETHOD.

  METHOD trigger_handle_end_of_list.
    RAISE EVENT alv_end_of_list
      EXPORTING
        eo_dyndoc_id = e_dyndoc_id.
  ENDMETHOD.

  METHOD set_row_height.
    ao_splitter->set_row_height( id     = 1
                                 height = 5 ).
  ENDMETHOD.

  METHOD html_display.
    IF ao_html_cntrl IS INITIAL.
      ao_html_cntrl = NEW #( ao_parent_html ).
    ENDIF.
    "Reuse_alv_grid_commentary_set
    CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
      EXPORTING
        document = io_dyndoc_id
        bottom   = space.
    "Get TOP->HTML_TABLE ready
    io_dyndoc_id->merge_document( ).
    "Connect TOP document to HTML-Control
    io_dyndoc_id->html_control = ao_html_cntrl.
    "Display TOP document
    io_dyndoc_id->display_document(
      EXPORTING
        reuse_control      = 'X'
        parent             = ao_parent_html
      EXCEPTIONS
        html_display_error = 1 ).
    IF sy-subrc NE 0.
      MESSAGE i398(00) WITH 'Error in displaying top-of-page'(e01).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
