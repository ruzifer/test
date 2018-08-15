CLASS zcl_ca_view_selscr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ca_view_selscr.
    "SALV
    ALIASES alv_user_command
      FOR zif_ca_view_selscr~alv_user_command.
    ALIASES alv_after_salv_function
      FOR zif_ca_view_selscr~alv_after_salv_function.
    ALIASES alv_link_click
      FOR zif_ca_view_selscr~alv_link_click.
    ALIASES alv_double_click
      FOR zif_ca_view_selscr~alv_double_click.
    ALIASES alv_top_of_page
      FOR zif_ca_view_selscr~alv_top_of_page.
    ALIASES alv_end_of_page
      FOR zif_ca_view_selscr~alv_end_of_page.

    ALIASES trigger_handle_user_command
      FOR zif_ca_view_selscr~trigger_handle_user_command.
    ALIASES trigger_handle_link_click
      FOR zif_ca_view_selscr~trigger_handle_link_click.
    ALIASES trigger_handle_double_click
      FOR zif_ca_view_selscr~trigger_handle_double_click.
    ALIASES display_alv
      FOR zif_ca_view_selscr~display_alv.
    ALIASES refresh_alv
      FOR zif_ca_view_selscr~refresh_alv.
    ALIASES set_handle_key
      FOR zif_ca_view_selscr~set_handle_key.
    "SALV TREE
    ALIASES alv_tree_double_click
      FOR zif_ca_view_selscr~alv_tree_double_click.
    ALIASES alv_tree_checkbox_chg
      FOR zif_ca_view_selscr~alv_tree_checkbox_chg.
    ALIASES handle_tree_user_command
      FOR zif_ca_view_selscr~handle_tree_user_command.
    ALIASES handle_tree_double_click
      FOR zif_ca_view_selscr~handle_tree_double_click.
    ALIASES handle_tree_checkbox_chg
      FOR zif_ca_view_selscr~handle_tree_checkbox_chg.
    ALIASES display_alv_tree
      FOR zif_ca_view_selscr~display_alv_tree.

  PROTECTED SECTION.

    DATA ao_alv TYPE REF TO cl_salv_table .
    DATA ao_tree TYPE REF TO cl_salv_tree .
    DATA at_tree TYPE zttca_tree_data .
    CONSTANTS cc_marked TYPE snodetext-text1 VALUE 'SYM_CHECKBOX' ##NO_TEXT.

    METHODS columns_desc
      IMPORTING
        VALUE(iv_column)  TYPE lvc_fname
        VALUE(iv_text)    TYPE scrtext_l
        VALUE(iv_tooltip) TYPE any OPTIONAL
      CHANGING
        !co_columns       TYPE REF TO cl_salv_columns_table .
    METHODS column_alignment
      IMPORTING
        VALUE(iv_column) TYPE lvc_fname
        VALUE(iv_align)  TYPE salv_de_alignment DEFAULT if_salv_c_alignment=>left
      CHANGING
        !co_columns      TYPE REF TO cl_salv_columns_table .
    METHODS column_hotspot
      IMPORTING
        VALUE(iv_column) TYPE lvc_fname
      CHANGING
        !co_columns      TYPE REF TO cl_salv_columns_table .
    METHODS column_invisible
      IMPORTING
        VALUE(iv_column)    TYPE lvc_fname
        VALUE(iv_invisible) TYPE boolean DEFAULT abap_true
      CHANGING
        !co_columns         TYPE REF TO cl_salv_columns_table .
    METHODS check_box
      IMPORTING
        VALUE(iv_column) TYPE lvc_fname
      CHANGING
        !co_columns      TYPE REF TO cl_salv_columns_table .
    METHODS set_alv_function
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS alv_columns
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS sort_alv
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS alv_layout
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS get_initial_layout
      RETURNING
        VALUE(rv_variant) TYPE slis_vari .
    METHODS set_display_setting
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS get_title_bar
      RETURNING
        VALUE(rv_title) TYPE lvc_title .
    METHODS register_events
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS set_top_of_page
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS set_end_of_page
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS set_screen_popup
      EXPORTING
        !ev_start_col   TYPE i
        !ev_end_col     TYPE i
        !ev_start_row   TYPE i
        !ev_end_row     TYPE i
      RETURNING
        VALUE(rv_popup) TYPE abap_bool .
    METHODS screen_popup
      IMPORTING
        !io_alv TYPE REF TO cl_salv_table .
    METHODS alv_tree_settings .
    METHODS register_tree_events .
    METHODS tree_columns .
    METHODS column_tree_invisible
      IMPORTING
        VALUE(iv_column)    TYPE lvc_fname
        VALUE(iv_invisible) TYPE boolean DEFAULT abap_true
      CHANGING
        !co_columns         TYPE REF TO cl_salv_columns_tree .
    METHODS add_nodes
      CHANGING
        !ct_nodetab TYPE zttca_nodetab .
  PRIVATE SECTION.
    DATA:
      av_program TYPE sy-repid,
      av_dynpro  TYPE sy-dynnr.
    METHODS create_node
      IMPORTING
        iv_parent  TYPE lvc_nkey
        iw_tree    TYPE zsca_tree_data
      CHANGING
        cd_nodetab TYPE REF TO snodetext.
    METHODS get_parent_node_key
      IMPORTING
        iv_nodetab_id      TYPE snodetext-id
        it_nodetab         TYPE zttca_nodetab
      RETURNING
        VALUE(rv_node_key) TYPE lvc_nkey.
    METHODS set_function_tree.
    METHODS expand_tree
      IMPORTING
        iv_expand TYPE abap_bool.
    METHODS initialize_tree
      IMPORTING
        iv_cusctrl_nm TYPE c OPTIONAL.
    METHODS refresh_tree_nodes.

ENDCLASS.



CLASS zcl_ca_view_selscr IMPLEMENTATION.

  METHOD alv_columns.
    DATA(lo_columns) = io_alv->get_columns( ).
    "Description
*    columns_desc( EXPORTING iv_column  = 'XXX'
*                            iv_text    = text-m01
*                  CHANGING  co_columns = lo_columns ).
    "optimize the columns
    lo_columns->set_optimize( ).
*          DATA(lo_aggrs) = mo_alv->get_aggregations( ).
*         Add TOTAL for COLUMN
*          TRY.
*                lo_aggrs->add_aggregation( columnname = '' ).
*            CATCH cx_salv_data_error .                  "#EC NO_HANDLER
*            CATCH cx_salv_not_found .                   "#EC NO_HANDLER
*            CATCH cx_salv_existing .                    "#EC NO_HANDLER
*          ENDTRY.
    "Set color
*    TRY.
*        lo_columns->set_color_column( 'T_CELL_COLOR' ).
*      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
*    ENDTRY.
  ENDMETHOD.


  METHOD alv_layout.
    "Get Layout
    DATA(lo_layout) = io_alv->get_layout( ).
    "Set Layout
    lo_layout->set_key( VALUE salv_s_layout_key( report = sy-cprog handle = set_handle_key( ) ) ).
    lo_layout->set_default( abap_true ).
    lo_layout->set_initial_layout( get_initial_layout( ) ).
    lo_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
  ENDMETHOD.


  METHOD alv_tree_settings.

  ENDMETHOD.


  METHOD check_box.
    TRY.
*       change the description of columns
        DATA(lo_column) = CAST cl_salv_column_list( co_columns->get_column( iv_column ) ).
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.                    "check_box


  METHOD columns_desc.
    TRY.
*       change the description of columns
        DATA(lo_column) = co_columns->get_column( iv_column ).
*       short text and long text blank. similarly applicable for short and long text overriding.
        lo_column->set_short_text( EXPORTING value = ' ' ).
        lo_column->set_medium_text( EXPORTING value = CONV scrtext_m( iv_text ) ).
        lo_column->set_long_text( EXPORTING value = iv_text ).

        IF iv_tooltip IS SUPPLIED.
          lo_column->set_tooltip( CONV lvc_tip( iv_tooltip ) ).
        ENDIF.
      CATCH cx_salv_not_found.

    ENDTRY.
  ENDMETHOD.                    "columns_desc


  METHOD column_alignment.
    TRY.
        DATA(lo_column) = CAST cl_salv_column_table( co_columns->get_column( iv_column ) ).
        lo_column->set_alignment( iv_align ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.                    "column_hotspot


  METHOD column_hotspot.
    TRY.
        DATA(lo_column) = CAST cl_salv_column_table( co_columns->get_column( iv_column ) ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.                    "column_hotspot


  METHOD column_invisible.
    TRY.
*       change the description of columns
        DATA(lo_column) = co_columns->get_column( iv_column ).
        lo_column->set_technical( iv_invisible ).

      CATCH cx_salv_not_found.

    ENDTRY.
  ENDMETHOD.                    "columns_visible


  METHOD column_tree_invisible.
    TRY.
*       change the description of columns
        DATA(lo_column) = co_columns->get_column( iv_column ).
        lo_column->set_technical( iv_invisible ).

      CATCH cx_salv_not_found.

    ENDTRY.
  ENDMETHOD.                    "columns_visible

  METHOD add_nodes.
    "Refresh node tree
    refresh_tree_nodes( ).

    "Start add node tree
    LOOP AT ct_nodetab REFERENCE INTO DATA(ld_nodetab).
      APPEND INITIAL LINE TO at_tree ASSIGNING FIELD-SYMBOL(<lf_tree>).
      <lf_tree>-tlevel = ld_nodetab->tlevel.
      <lf_tree>-text1  = ld_nodetab->text3.
      <lf_tree>-hide   = ld_nodetab->id.
*      <lf_tree>-text2  = <lf_nodetab>-text3.

      create_node(
        EXPORTING
          iv_parent     = get_parent_node_key( iv_nodetab_id = ld_nodetab->parent it_nodetab = ct_nodetab )
          iw_tree       = <lf_tree>
        CHANGING
          cd_nodetab    = ld_nodetab ).
    ENDLOOP.
  ENDMETHOD.

  METHOD create_node.
*...working with nodes
    DATA(lo_nodes) = ao_tree->get_nodes( ).
    TRY.
        DATA(lo_node) = lo_nodes->add_node(
                        related_node   = iv_parent
                        relationship   = cl_gui_column_tree=>relat_last_child
                        data_row       = iw_tree
*                    collapsed_icon =
*                    expanded_icon  =
*                    row_style      =
*                    text           =
*                    visible        = ABAP_TRUE
*                    expander       =
*                    enabled        = ABAP_TRUE
*                    folder         = SWITCH #( cw_nodetab-text1 WHEN cc_marked THEN abap_true )
                    ).
        lo_node->set_text( CONV #( cd_nodetab->text2 ) ).
        IF cd_nodetab->text1 EQ cc_marked.
          DATA(lo_item) = lo_node->get_hierarchy_item( ).
          lo_item->set_type( if_salv_c_item_type=>checkbox ).
*          lo_item->set_checked( abap_true ).
          lo_item->set_editable( abap_true ).
        ENDIF.
*                  CATCH cx_salv_msg.  "
        cd_nodetab->name = lo_node->get_key( ).
      CATCH cx_salv_msg.  "
    ENDTRY.
  ENDMETHOD.


  METHOD display_alv.
    IF lines( ct_data ) > 0.
      TRY.
          "Create ALV instance
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              container_name = iv_cusctrl_nm
            IMPORTING
              r_salv_table   = ao_alv
            CHANGING
              t_table        = ct_data.
          "Set screen popup
          screen_popup( ao_alv ).
          "Set functions objects
          set_alv_function( ao_alv ).
          "Set column object
          alv_columns( ao_alv ).
          "Sort ALV
          sort_alv( ao_alv ).
          "Set layout
          alv_layout( ao_alv ).
          "Set Display setting
          set_display_setting( ao_alv ).
          "Register events
          register_events( ao_alv ).
          "Set top of page
          set_top_of_page( ao_alv ).
          "Set end of page
          set_end_of_page( ao_alv ).
          "Set color
          ao_alv->display( ).
          "Catch exceptions
        CATCH cx_salv_msg  INTO DATA(lo_error).
          MESSAGE lo_error->get_text( ) TYPE 'E'.
      ENDTRY.

    ELSE.
      MESSAGE |No data found.| TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD expand_tree.
    IF iv_expand EQ abap_true.
      DATA(lo_nodes) = ao_tree->get_nodes( ).
      lo_nodes->expand_all( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_initial_layout.
*  The implementation must be made in subclass method if necessary !
  ENDMETHOD.


  METHOD get_parent_node_key.
    TRY.
        rv_node_key = it_nodetab[ id = iv_nodetab_id ]-name.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD get_title_bar.
*  The implementation must be made in subclass method if necessary !
  ENDMETHOD.


  METHOD refresh_alv.
    IF ct_table IS SUPPLIED.
      TRY.
          CALL METHOD ao_alv->set_data
            CHANGING
              t_table = ct_table.
        CATCH cx_salv_no_new_data_allowed INTO DATA(lo_error).
          MESSAGE lo_error->get_text( ) TYPE 'E'.
      ENDTRY.
    ENDIF.

    ao_alv->refresh( s_stable     = iw_stable
                     refresh_mode = iv_refresh_mode ).

    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "refresh_alv


  METHOD register_events.
*  The implementation must be made in subclass method if necessary !
    "Example
    "Register events
    "DATA(lo_events) = io_alv->get_event( ).
    "register to the events
    "SET HANDLER on_user_command on_link_click on_double_click FOR lo_events.
  ENDMETHOD.


  METHOD register_tree_events.
    "Register events
    DATA(lo_events) = ao_tree->get_event( ).
    "register to the events
    SET HANDLER handle_tree_user_command handle_tree_double_click handle_tree_checkbox_chg FOR lo_events.
  ENDMETHOD.


  METHOD screen_popup.
    set_screen_popup(
      IMPORTING
        ev_start_col = DATA(lv_start_col)
        ev_end_col   = DATA(lv_end_col)
        ev_start_row = DATA(lv_start_row)
        ev_end_row   = DATA(lv_end_row)
      RECEIVING
        rv_popup     = DATA(lv_popup) ).
    IF lv_popup EQ abap_true.
      io_alv->set_screen_popup(
        start_column = lv_start_col
        end_column   = lv_end_col
        start_line   = lv_start_row
        end_line     = lv_end_row ).
    ENDIF.
  ENDMETHOD.


  METHOD set_alv_function.
    "Standard PF-Status
    TRY.
*       Get functions object
        DATA(lo_functions) = io_alv->get_functions( ).
        lo_functions->set_all( ).
      CATCH cx_salv_wrong_call cx_salv_not_found.       "#EC NO_HANDLER
    ENDTRY.
*    "Custom PF-Status
*    io_alv->set_screen_status(
*      pfstatus      =  'SALV_STANDARD'
*      report        =  'SALV_DEMO_TABLE_SELECTIONS'
*      set_functions = io_alv->c_functions_all ).
  ENDMETHOD.


  METHOD set_display_setting.
    "Get Display setting
    DATA(lo_display) = io_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
    "Title bar
    lo_display->set_list_header( get_title_bar( ) ).
  ENDMETHOD.


  METHOD set_end_of_page.
    RAISE EVENT alv_end_of_page
      EXPORTING
        eo_alv = io_alv.
*  The implementation must be made in subclass method if necessary !
*    "Example
**   Footer object
*    DATA(lo_footer) = NEW cl_salv_form_layout_grid( ).
*
**   information in Bold
*    DATA(lo_f_label) = lo_footer->create_label( row = 1 column = 1 ).
*    lo_f_label->set_text( 'Header in Bold' ).
*
**   information in tabular format
*    DATA(lo_f_flow) = lo_footer->create_flow( row = 2  column = 1 ).
*    lo_f_flow->create_text( text = 'This is text of flow' ).
*
**   set the end of list using the header for Online.
*    io_alv->set_end_of_list( lo_footer ).
*
**   set the end of list using the header for Print.
*    io_alv->set_end_of_list_print( lo_footer ).
  ENDMETHOD.


  METHOD set_function_tree.
    DATA(lo_functions) = ao_tree->get_functions( ).
    lo_functions->set_all( abap_true ).
    TRY.
        lo_functions->set_function(
          EXPORTING
            name     =  'FLAT'
            boolean  =  space ).
      CATCH cx_salv_not_found cx_salv_wrong_call.
    ENDTRY.
  ENDMETHOD.


  METHOD set_screen_popup.
*  The implementation must be made in subclass method if necessary !
    "Example
*    rv_popup     = abap_true.
*    ev_start_col = 1.
*    ev_end_col   = 100.
*    ev_start_row = 1.
*    ev_end_row   = 20.
  ENDMETHOD.


  METHOD set_top_of_page.
    RAISE EVENT alv_top_of_page
      EXPORTING
        eo_alv = io_alv.
*  The implementation must be made in subclass method if necessary !
*    "Example
**   header object
*    DATA(lo_header) = NEW cl_salv_form_layout_grid( ).
*
**   information in Bold
*    DATA(lo_h_label) = lo_header->create_label( row = 1 column = 1 ).
*    lo_h_label->set_text( 'Header in Bold' ).
*
**   information in tabular format
*    DATA(lo_h_flow) = lo_header->create_flow( row = 1  column = 2 ).
*    lo_h_flow->create_text( text = 'This is text of flow' ).
*
**   set the top of list using the header for Online.
*    io_alv->set_top_of_list( lo_header ).
*
**   set the top of list using the header for Print.
*    io_alv->set_top_of_list_print( lo_header ).
  ENDMETHOD.


  METHOD sort_alv.
*  The implementation must be made in subclass method if necessary !
    "Example
    "Get sort column
*    DATA(lo_sorts) = io_alv->get_sorts( ).
*    lo_sorts->clear( ).
    "Sort table
*    lo_sorts->add_sort( columnname = '' ).
  ENDMETHOD.


  METHOD tree_columns.
    DATA(lo_columns) = ao_tree->get_columns( ).
    "Description
*    columns_desc( EXPORTING iv_column  = 'XXX'
*                            iv_text    = text-m01
*                  CHANGING  co_columns = lo_columns ).
    column_tree_invisible(
      EXPORTING
        iv_column    = 'TLEVEL'
      CHANGING
        co_columns   = lo_columns ).
    column_tree_invisible(
      EXPORTING
        iv_column    = 'TEXT2'
      CHANGING
        co_columns   = lo_columns ).
    column_tree_invisible(
      EXPORTING
        iv_column    = 'TEXT3'
      CHANGING
        co_columns   = lo_columns ).
    column_tree_invisible(
      EXPORTING
        iv_column    = 'HIDE'
      CHANGING
        co_columns   = lo_columns ).
    "optimize the columns
    lo_columns->set_optimize( ).

  ENDMETHOD.


  METHOD trigger_handle_double_click.
    RAISE EVENT alv_double_click
      EXPORTING
        ev_row    = row
        ev_column = column.
  ENDMETHOD.


  METHOD trigger_handle_link_click.
    RAISE EVENT alv_link_click
      EXPORTING
        ev_row    = row
        ev_column = column.
  ENDMETHOD.


  METHOD trigger_handle_user_command.
    RAISE EVENT alv_user_command
      EXPORTING
        ev_salv_function = e_salv_function.
  ENDMETHOD.                    "on_user_command


  METHOD display_alv_tree.
    IF lines( ct_nodetab ) > 0.
      "Initial tree
      initialize_tree( ).
      "Add node in tree
      add_nodes(
        CHANGING
          ct_nodetab = ct_nodetab ).
      "expand tree
      expand_tree( iv_expand ).
      "Display tree
      ao_tree->display( ).
    ELSE.
      MESSAGE |No data found.| TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD handle_tree_checkbox_chg.
    RAISE EVENT alv_tree_checkbox_chg
      EXPORTING
        iv_checked  = checked
        iv_column   = columnname
        iv_node_key = node_key.
  ENDMETHOD.


  METHOD handle_tree_double_click.

  ENDMETHOD.


  METHOD handle_tree_user_command.
    RAISE EVENT alv_user_command
      EXPORTING
        ev_salv_function = e_salv_function.
  ENDMETHOD.


  METHOD zif_ca_view_selscr~trigger_handle_after_salv_func.
    RAISE EVENT alv_after_salv_function
      EXPORTING
        ev_salv_function = e_salv_function.
  ENDMETHOD.

  METHOD initialize_tree.
    IF ao_tree IS BOUND.
      RETURN.
    ENDIF.
    DATA(lo_container) = COND #( WHEN iv_cusctrl_nm IS INITIAL THEN cl_gui_container=>default_screen
                                 ELSE NEW cl_gui_custom_container( iv_cusctrl_nm ) ).
    "initialize tree data
    CLEAR at_tree.

    TRY.
        "Create ALV instance
        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = lo_container
          IMPORTING
            r_salv_tree = ao_tree
          CHANGING
            t_table     = at_tree.
      CATCH cx_salv_error  INTO DATA(lo_error).
        MESSAGE lo_error->get_text( ) TYPE 'E'.
    ENDTRY.
    tree_columns( ).
    "Set functions objects
    set_function_tree( ).
*          "Sort ALV
*          sort_alv( ).
*          "Set layout
*          alv_layout( ).
*          "Set Display setting
*          set_display_setting( ).
    "Register events
    register_tree_events( ).

  ENDMETHOD.


  METHOD refresh_tree_nodes.
    TRY.
        DATA(lo_nodes) = ao_tree->get_nodes( ).
        lo_nodes->delete_all( ).
      CATCH cx_salv_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
