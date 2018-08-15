class ZCL_CA_UTILITY_EXCEL definition
  public
  final
  create public .

public section.

  class-data MO_OBJECT type ref to ZCL_CA_UTILITY_EXCEL .

  methods GEN_EXCEL_FILE
    importing
      value(IO_DATA) type ref to DATA
      value(IV_NAME) type STRING
      value(IV_LAYO) type DISVARIANT-VARIANT optional
      value(IV_PROG) type SY-CPROG default SY-CPROG
      value(IV_HANDLE) type SALV_S_LAYOUT_KEY-HANDLE optional
      value(IT_FCAT) type LVC_T_FCAT optional
    exporting
      value(ET_XML_TABLE) type SOLIX_TAB .
  methods GET_FCAT_BY_LAYOUT
    importing
      value(IV_PROG) type SY-CPROG
      value(IV_LAYO) type DISVARIANT-VARIANT
      value(IV_HANDLE) type SALV_S_LAYOUT_KEY-HANDLE
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods GET_INSTANCE
    returning
      value(RO_OBJECT) type ref to ZCL_CA_UTILITY_EXCEL .
  methods SEND_EMAIL
    importing
      value(IV_NAME) type STRING
      value(IV_FLNAME_HEX) type STRING optional
      value(IV_FLNAME_TEXT) type STRING optional
      value(IT_INTERNAL_USER) type ZTTCAT_UNAME optional
      value(IT_EXTERNAL_EMAIL) type BCSY_SMTPA optional
      value(IT_BODYTEXT) type SOLI_TAB optional
      value(IT_CONTENT_HEX) type SOLIX_TAB optional
      value(IT_CONTENT_TEXT) type SOLI_TAB optional .
  methods SELECTION_SCREEN
    importing
      value(IV_REPID) type SY-CPROG default SY-CPROG
      value(IT_FLDHIDE) type FIELDNAME_T optional
    returning
      value(RT_TEXT) type SOLI_TAB .
protected section.
private section.

  types:
    cty_t_rsel_info TYPE STANDARD TABLE OF rsel_info WITH DEFAULT KEY .

  data MO_IXML type ref to IF_IXML .
  data MO_STREAMFACTORY type ref to IF_IXML_STREAM_FACTORY .
  data MO_OSTREAM type ref to IF_IXML_OSTREAM .
  data MO_RENDERER type ref to IF_IXML_RENDERER .
  data MO_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data MO_ATTRIBUTE type ref to IF_IXML_ATTRIBUTE .
  data MO_ELEMENT_ROOT type ref to IF_IXML_ELEMENT .
  data MO_ELEMENT_PROPERTIES type ref to IF_IXML_ELEMENT .
  data MO_STYLES type ref to IF_IXML_ELEMENT .
  data MO_STYLE type ref to IF_IXML_ELEMENT .
  data MO_FORMAT type ref to IF_IXML_ELEMENT .
  data MO_BORDER type ref to IF_IXML_ELEMENT .
  data MO_WORKSHEET type ref to IF_IXML_ELEMENT .
  data MO_TABLE type ref to IF_IXML_ELEMENT .
  data MO_COLUMN type ref to IF_IXML_ELEMENT .
  data MO_ROW type ref to IF_IXML_ELEMENT .
  data MO_CELL type ref to IF_IXML_ELEMENT .
  data MO_DATA type ref to IF_IXML_ELEMENT .
  constants MC_RELID type LTDX-RELID value 'LT' ##NO_TEXT.
  data MT_INFO type CTY_T_RSEL_INFO .
  constants MC_REFVAL type CHAR255 value 'D       .' ##NO_TEXT.
  constants MC_MAXLENGTH type I value 255 ##NO_TEXT.

  methods CRT_XML_DOCUMENT .
  methods DOCUMENT_PROPERTIES
    importing
      value(IV_FILENM) type STRING .
  methods DOCUMENT_STYLE .
  methods SET_STYLE
    importing
      value(IV_STYNM) type STRING optional
      value(IV_FORNM) type STRING optional
      value(IV_NAME) type STRING
      value(IV_VAL) type STRING .
  methods DRAW_BORDERS .
  methods CREATE_WORKSHEET
    importing
      value(IV_NAME) type STRING .
  methods FILL_HEADER .
  methods FILL_DETAIL
    importing
      value(IO_DATA) type ref to DATA
      value(IV_LAYO) type DISVARIANT-VARIANT optional
      value(IV_PROG) type SY-CPROG optional
      value(IV_HANDLE) type SALV_S_LAYOUT_KEY-HANDLE
      value(IT_FCAT) type LVC_T_FCAT optional .
  methods FILL_VALUE
    importing
      value(IV_STYNM) type STRING optional
      value(IV_VAL) type STRING .
  methods SAVE_XML_DOCUMENT
    exporting
      value(ET_XML_TABLE) type SOLIX_TAB .
  methods DRAW_BORDER
    importing
      value(IV_SIDE) type STRING .
  methods CONV_OUT
    importing
      value(IV_DATA) type ANY
      value(IV_CURR) type WAERS optional
      value(IV_QUAN) type MEINS optional
    returning
      value(RV_DATA) type STRING .
  methods GET_FCAT
    importing
      value(IT_DATA) type TABLE
      value(IV_LAYO) type DISVARIANT-VARIANT optional
      value(IV_PROG) type SY-CPROG optional
      value(IV_HANDLE) type SALV_S_LAYOUT_KEY-HANDLE optional
      value(IT_FCAT) type LVC_T_FCAT optional
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods GET_DECIMAL
    importing
      !IV_CURR type WAERS
    returning
      value(RV_DECIMAL) type I .
  methods SELVAL
    importing
      value(IS_SELECTION) type RSPARAMS
    returning
      value(RV_TEXT) type TEXT255 .
  methods FORMAT_OUT
    importing
      value(IV_VALUE) type CLIKE
      value(IV_TYPE) type CLIKE
    returning
      value(RV_OUT) type TEXT255 .
  methods GET_SCREEN_INFO
    importing
      !IV_REPID type SY-CPROG
    returning
      value(RT_INFO) type CTY_T_RSEL_INFO .
  methods READ_DICT_TEXT
    importing
      !IV_SELNAME type RSPARAMS-SELNAME
    returning
      value(RV_TEXT) type AS4TEXT .
  methods GET_FIELD_INFO
    importing
      value(IV_TABNAME) type DDOBJNAME
      value(IV_FIELDNAME) type FIELDNAME
    returning
      value(RS_DFIES) type DFIES .
ENDCLASS.



CLASS ZCL_CA_UTILITY_EXCEL IMPLEMENTATION.


  METHOD CONV_OUT.
    DATA: lv_data TYPE text255,
          lv_type TYPE char1.

    DESCRIBE FIELD iv_data EDIT MASK DATA(lv_mask) TYPE lv_type.
    CASE lv_type.
      WHEN 'N'.
        WRITE iv_data NO-ZERO TO lv_data.
        rv_data = CONV string( condense( lv_data ) ).
      WHEN 'P'.
        IF iv_curr IS SUPPLIED .
          WRITE iv_data TO lv_data CURRENCY iv_curr .
        ELSEIF iv_quan IS SUPPLIED .
          WRITE iv_data TO lv_data UNIT iv_quan .
        ELSE .
          WRITE iv_data TO lv_data .
        ENDIF.

        rv_data = lv_data .

        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            value = rv_data.
      WHEN OTHERS.
        IF lv_mask IS NOT INITIAL.
          DATA(lv_fmname) = |CONVERSION_EXIT_{ lv_mask+2 }_OUTPUT|.
          CALL FUNCTION lv_fmname
            EXPORTING
              input  = iv_data
            IMPORTING
              output = rv_data
            EXCEPTIONS
              OTHERS = 8
              ##fm_subrc_ok.
        ELSE.
          CHECK iv_data IS NOT INITIAL.
          WRITE iv_data LEFT-JUSTIFIED TO lv_data.
          rv_data = CONV string( lv_data ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD CREATE_WORKSHEET.
    "Worksheet
    mo_worksheet = mo_document->create_simple_element( name   = 'Worksheet'
                                                       parent = mo_element_root ).
    mo_worksheet->set_attribute_ns( name   = 'Name'
                                    prefix = 'ss'
                                    value  = iv_name ).
    "Table
    mo_table = mo_document->create_simple_element( name = 'Table'
                                                   parent = mo_worksheet ).
    mo_table->set_attribute_ns( name   = 'FullColumns'
                                prefix = 'x'
                                value  = '1' ).
    mo_table->set_attribute_ns( name   = 'FullRows'
                                prefix = 'x'
                                value  = '1' ).
  ENDMETHOD.


  METHOD CRT_XML_DOCUMENT.
    "Creating a ixml Factory
    mo_ixml = cl_ixml=>create( ).
    "Creating the DOM Object Model
    mo_document = mo_ixml->create_document( ).
    "Create Root Node 'Workbook'
    mo_element_root = mo_document->create_simple_element( name   = 'Workbook'
                                                          parent = mo_document ).
    mo_element_root->set_attribute( name  = 'xmlns'
                                    value = 'urn:schemas-microsoft-com:office:spreadsheet' ).
    mo_attribute = mo_document->create_namespace_decl( name   = 'ss'
                                                       prefix = 'xmlns'
                                                       uri    = 'urn:schemas-microsoft-com:office:spreadsheet' ).
    mo_element_root->set_attribute_node( mo_attribute ).
    mo_attribute = mo_document->create_namespace_decl( name   = 'x'
                                                       prefix = 'xmlns'
                                                       uri    = 'urn:schemas-microsoft-com:office:excel' ).
    mo_element_root->set_attribute_node( mo_attribute ).
    mo_attribute = mo_document->create_namespace_decl( name   = 'x'
                                                       prefix = 'xmlns'
                                                       uri    = 'urn:schemas-microsoft-com:office:excel' ).
    mo_element_root->set_attribute_node( mo_attribute ).
  ENDMETHOD.


  METHOD DOCUMENT_PROPERTIES.
    "Create node for document properties.
    SPLIT iv_filenm AT space INTO TABLE DATA(lt_text).
    CLEAR iv_filenm.
    LOOP AT lt_text INTO DATA(ls_text).
      iv_filenm = |{ iv_filenm }{ ls_text }|.
    ENDLOOP.

    mo_element_properties = mo_document->create_simple_element( name   = iv_filenm
                                                                parent = mo_element_root ).
    mo_document->create_simple_element( name   = 'Author'
                                        value  = CONV string( sy-uname )
                                        parent = mo_element_properties ).
  ENDMETHOD.


  METHOD DOCUMENT_STYLE.
    "Styles
    mo_styles = mo_document->create_simple_element( name = 'Styles' parent = mo_element_root ).
    "Style for Header
    set_style( iv_stynm = 'HC' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Center' ).
    set_style( iv_fornm = 'Interior' iv_name = 'Color' iv_val = '#FFFF00' ).
    set_style( iv_name = 'Pattern' iv_val = 'Solid' ).
    set_style( iv_fornm = 'Font' iv_name = 'Bold' iv_val = '1' ).
    "Border
    draw_borders( ).
    CLEAR mo_style.
    "Style for Normal
    set_style( iv_stynm = 'NL' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Left' ).
    draw_borders( ).
    CLEAR mo_style.
    "Style for standard decimal number
    set_style( iv_stynm = 'PS' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Right' ).
    set_style( iv_fornm = 'NumberFormat' iv_name = 'Format' iv_val = 'Standard' ).
    draw_borders( ).
    CLEAR mo_style.

    "Style for Non-decimal number
    set_style( iv_stynm = 'P0' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Right' ).
    set_style( iv_fornm = 'NumberFormat' iv_name = 'Format' iv_val = '#,##0' ).
    draw_borders( ).
    CLEAR mo_style.

    "Style for 3 digit decimal number
    set_style( iv_stynm = 'P3' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Right' ).
    set_style( iv_fornm = 'NumberFormat' iv_name = 'Format' iv_val = '#,##0.000' ).
    draw_borders( ).
    CLEAR mo_style.

    "Style for 5 digit decimal number
    set_style( iv_stynm = 'P5' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Right' ).
    set_style( iv_fornm = 'NumberFormat' iv_name = 'Format' iv_val = '#,##0.00000' ).
    draw_borders( ).
    CLEAR mo_style.

    "Style for number
    set_style( iv_stynm = 'P' iv_fornm = 'Alignment' iv_name = 'Horizontal' iv_val = 'Right' ).
    draw_borders( ).
    CLEAR mo_style.
  ENDMETHOD.


  METHOD DRAW_BORDER.
    mo_format = mo_document->create_simple_element( name = 'Border'  parent = mo_border ).
    mo_format->set_attribute_ns( name = 'Weight'    prefix = 'ss' value = '1' ).
    mo_format->set_attribute_ns( name = 'LineStyle' prefix = 'ss' value = 'Continuous' ).
    mo_format->set_attribute_ns( name = 'Position'  prefix = 'ss' value = iv_side ).
  ENDMETHOD.


  method DRAW_BORDERS.
    mo_border = mo_document->create_simple_element( name = 'Borders' parent = mo_style ).
    draw_border( 'Bottom' ).
    draw_border( 'Left' ).
    draw_border( 'Right' ).
    draw_border( 'Top' ).
  endmethod.


  METHOD FILL_DETAIL.
    FIELD-SYMBOLS <lfs_tab> TYPE STANDARD TABLE.

    ASSIGN io_data->* TO <lfs_tab>.
    "Prepare field catalog
    DATA(lt_fcat) = get_fcat( it_data    = <lfs_tab>
                              iv_layo    = iv_layo
                              iv_prog    = iv_prog
                              iv_handle  = iv_handle
                              it_fcat    = it_fcat ).
*   Column Width
    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
      mo_column = mo_document->create_simple_element( name = 'Column' parent = mo_table ).
      DATA(lv_len) = COND i( WHEN strlen( <lfs_fcat>-reptext ) > <lfs_fcat>-outputlen THEN strlen( <lfs_fcat>-reptext )
                             ELSE <lfs_fcat>-outputlen ) * 5.
      mo_column->set_attribute_ns( name = 'Width' prefix = 'ss' value = |{ lv_len }| ).
      mo_column->set_attribute_ns( name = 'AutoFitWidth' prefix = 'ss' value = '1' ).
    ENDLOOP.

    "Meta data
    mo_row = mo_document->create_simple_element( name = 'Row' parent = mo_table ).
    LOOP AT lt_fcat ASSIGNING <lfs_fcat>.
      fill_value( iv_val = CONV string( <lfs_fcat>-reptext )
                  iv_stynm = 'HC' ).
    ENDLOOP.
    "Detail
    LOOP AT <lfs_tab> ASSIGNING FIELD-SYMBOL(<lfs_wa>).
      mo_row = mo_document->create_simple_element( name = 'Row' parent = mo_table ).
      LOOP AT lt_fcat ASSIGNING <lfs_fcat>.
        ASSIGN COMPONENT <lfs_fcat>-fieldname  OF STRUCTURE <lfs_wa> TO FIELD-SYMBOL(<lfs_comp>).
        ASSIGN COMPONENT <lfs_fcat>-cfieldname OF STRUCTURE <lfs_wa> TO FIELD-SYMBOL(<lfs_curr>).
        ASSIGN COMPONENT <lfs_fcat>-qfieldname OF STRUCTURE <lfs_wa> TO FIELD-SYMBOL(<lfs_quan>).
        DESCRIBE FIELD <lfs_comp> TYPE DATA(lv_type).
        IF <lfs_curr> IS ASSIGNED .
          DATA(lv_val) = conv_out( iv_data = <lfs_comp>
                                   iv_curr = <lfs_curr> ) .
          fill_value( iv_val = lv_val
                      iv_stynm = SWITCH #( lv_type WHEN 'P'
                                                   THEN SWITCH #( get_decimal( <lfs_curr> ) WHEN '0' THEN 'P0'
                                                                                            WHEN '3' THEN 'P3'
                                                                                            WHEN '5' THEN 'P5'
                                                                                            ELSE 'PS' )
                                                   ELSE 'NL' ) ).
          UNASSIGN <lfs_curr> .
        ELSEIF <lfs_quan> IS ASSIGNED .
          lv_val = conv_out( iv_data = <lfs_comp>
                             iv_quan = <lfs_quan> ) .
          fill_value( iv_val = lv_val
                      iv_stynm = SWITCH #( lv_type WHEN 'P' THEN 'P'
                                                   ELSE 'NL' ) ).
          UNASSIGN <lfs_quan> .
        ELSE .
          lv_val = conv_out( iv_data = <lfs_comp> ) .
          fill_value( iv_val = lv_val
                      iv_stynm = SWITCH #( lv_type WHEN 'P' THEN 'P'
                                                   ELSE 'NL' ) ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  method FILL_HEADER.
  endmethod.


  METHOD FILL_VALUE.
    mo_cell = mo_document->create_simple_element( name = 'Cell' parent = mo_row ).

    IF iv_stynm IS SUPPLIED.
      mo_cell->set_attribute_ns( name = 'StyleID' prefix = 'ss'  value = iv_stynm ).
    ENDIF.
    mo_data = mo_document->create_simple_element( name = 'Data' value = iv_val parent = mo_cell ). " Data

    CASE iv_stynm.
      WHEN 'P' OR 'P0' OR 'P3' OR 'P5' OR 'PS'.
        mo_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'Number' ). " Cell format
      WHEN OTHERS.
        mo_data->set_attribute_ns( name = 'Type' prefix = 'ss' value = 'String' ). " Cell format
    ENDCASE.

  ENDMETHOD.


  METHOD FORMAT_OUT.
    rv_out = COND #( WHEN iv_type = 'D' THEN |{ CONV datum( iv_value ) DATE =  USER }|
                     WHEN iv_type = 'T' THEN |{ CONV uzeit( iv_value ) TIME =  USER }|
                     ELSE iv_value ).
  ENDMETHOD.


  method GEN_EXCEL_FILE.
    "Create XML Document
    crt_xml_document( ).
    "Create document properties.
    document_properties( |{ iv_name }_{ sy-datlo }_{ sy-timlo }| ).
    "Create style
    document_style( ).
    "Add worksheet
    create_worksheet( iv_name ).
    "Header
    fill_header( ).
    "Detail
    fill_detail( io_data    = io_data
                 iv_layo    = iv_layo
                 iv_prog    = iv_prog
                 iv_handle  = iv_handle
                 it_fcat    = it_fcat ).
    "Save XML Document
    save_xml_document( IMPORTING et_xml_table = et_xml_table ).
  endmethod.


  METHOD GET_DECIMAL.
    SELECT SINGLE currdec
      INTO rv_decimal
      FROM tcurx
      WHERE currkey = iv_curr .
    IF sy-subrc <> 0 .
      rv_decimal = 2 . "Default decimal
    ENDIF.
  ENDMETHOD.


  METHOD GET_FCAT.
    "Get field cat
    IF lines( it_fcat ) > 0.
      rt_fcat = it_fcat.
    ELSE.
      TRY.
          cl_salv_table=>factory(
            EXPORTING
              list_display = abap_false
            IMPORTING
              r_salv_table = DATA(lo_salv)
            CHANGING
              t_table      = it_data ).
        CATCH cx_salv_msg.
      ENDTRY.
      DATA(lo_columns)  = lo_salv->get_columns( ).

      rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( EXPORTING r_columns      = lo_columns
                                                                             r_aggregations = lo_salv->get_aggregations( ) ).
    ENDIF.
    IF iv_layo   IS NOT INITIAL AND
       iv_prog   IS NOT INITIAL.
      get_fcat_by_layout( EXPORTING iv_prog   = iv_prog
                                    iv_layo   = iv_layo
                                    iv_handle = iv_handle
                          CHANGING  ct_fcat   = rt_fcat ).

    ENDIF.
  ENDMETHOD.


  METHOD GET_FCAT_BY_LAYOUT.
    DATA: lt_dbfieldcat TYPE STANDARD TABLE OF ltdxdata WITH DEFAULT KEY,
          lv_length     TYPE lvc_outlen.
    SELECT *
      INTO @DATA(ls_ltdx)
      UP TO 1 ROWS
      FROM ltdx
     WHERE relid   EQ @mc_relid
       AND report  EQ @iv_prog
       AND handle  EQ @iv_handle
       AND variant EQ @iv_layo.
    ENDSELECT.

    IF ls_ltdx IS NOT INITIAL.
      DATA(ls_varkey) = CORRESPONDING ltdxkey( ls_ltdx ).
      CALL FUNCTION 'LT_DBDATA_READ_FROM_LTDX'
        EXPORTING
          is_varkey    = ls_varkey
        TABLES
          t_dbfieldcat = lt_dbfieldcat
        EXCEPTIONS
          not_found    = 1
          wrong_relid  = 2.
      CHECK sy-subrc = 0.
      IF lines( lt_dbfieldcat ) > 0.
        DATA(lt_fcat) = ct_fcat.
        REFRESH ct_fcat.
        LOOP AT lt_dbfieldcat INTO DATA(ls_dbfcatgrp) GROUP BY ( key1 = ls_dbfcatgrp-key1 ).
          LOOP AT GROUP ls_dbfcatgrp ASSIGNING FIELD-SYMBOL(<lfs_dbfcat>)
            WHERE param EQ 'COL_POS'
               OR param EQ 'NO_OUT'
               OR param EQ 'OUTPUTLEN'.
            CASE <lfs_dbfcat>-param.
              WHEN 'COL_POS'.
                DATA(lv_col) = CONV i( <lfs_dbfcat>-value ).
              WHEN 'NO_OUT'.
                IF <lfs_dbfcat>-value EQ space.
                  DATA(ls_fcat) = VALUE #( lt_fcat[ fieldname = <lfs_dbfcat>-key1 ] OPTIONAL ).
                  ls_fcat-outputlen = lv_length.
                ELSE.
                  CLEAR ls_fcat.
                  EXIT.
                ENDIF.
              WHEN 'OUTPUTLEN'.
                lv_length = <lfs_dbfcat>-value.
            ENDCASE.
          ENDLOOP.
          IF ls_fcat IS NOT INITIAL.
            INSERT ls_fcat INTO ct_fcat INDEX lv_col.
          ENDIF.
          CLEAR ls_fcat.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD GET_FIELD_INFO.
    DATA: lt_dfies_tab TYPE dfies_tab.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tabname    " Name of the Table (of the Type) for which Information is Req
        fieldname      = iv_fieldname    " Use Parameter LFIELDNAME Instead
      TABLES
        dfies_tab      = lt_dfies_tab    " Field List if Necessary
      EXCEPTIONS
        not_found      = 1
        internal_error = 2.
    IF sy-subrc EQ 0.
      rs_dfies = VALUE #( lt_dfies_tab[ 1 ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD GET_INSTANCE.
    IF mo_object IS NOT BOUND.
      mo_object = NEW zcl_ca_utility_excel( ).
    ENDIF.
    ro_object = mo_object.
  ENDMETHOD.


  METHOD GET_SCREEN_INFO.
    DATA: lt_field_names TYPE STANDARD TABLE OF rsdynpar WITH DEFAULT KEY.

    CALL FUNCTION 'RS_REPORTSELECTSCREEN_INFO'
      EXPORTING
        report              = iv_repid        " Report Name
      TABLES
        field_info          = rt_info         " Information about type, reference field, etc.
        field_names         = lt_field_names
      EXCEPTIONS
        no_selections       = 1
        report_not_existent = 2
        subroutine_pool     = 3.
    IF sy-subrc NE 0.
      "nothing
    ENDIF.
  ENDMETHOD.


  METHOD READ_DICT_TEXT.
    SPLIT mt_info[ name = iv_selname ]-dbfield AT '-' INTO DATA(lv_tab) DATA(lv_fld).
    IF lv_tab IS NOT INITIAL AND lv_tab IS NOT INITIAL.
      rv_text = get_field_info( iv_tabname   = CONV ddobjname( lv_tab )
                                iv_fieldname = CONV fieldname( lv_fld ) )-fieldtext.
    ENDIF.
  ENDMETHOD.


  METHOD SAVE_XML_DOCUMENT.
    "Creating a Stream Factory
    mo_streamfactory = mo_ixml->create_stream_factory( ).
    "Connect Internal XML Table to Stream Factory
    mo_ostream = mo_streamfactory->create_ostream_itable( table = et_xml_table ).
    "Rendering the Document
    mo_renderer = mo_ixml->create_renderer( ostream = mo_ostream document = mo_document ).
    DATA(lv_rc) = mo_renderer->render( ).
    "Saving the XML Document
    DATA(lv_xml_size) = mo_ostream->get_num_written_raw( ).
  ENDMETHOD.


  METHOD SELECTION_SCREEN.
    DATA: lt_selection_table     TYPE rsparams_tt,
          lt_selection_table_255 TYPE pivb_rsparamsl_255_t,
          lt_textpool            TYPE textpool_table,
          lv_text                TYPE text255,
          lv_val                 TYPE string,
          lv_texttmp             TYPE string,
          lv_offset              TYPE i.

    DATA(lr_fldhide)       = VALUE hrtnm_tab_rng_fieldname( FOR wa IN it_fldhide LET s = 'E' o = 'EQ' IN ( sign = s option = o low = wa ) ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = iv_repid                   " Program for which selections are to be displayed
      TABLES
        selection_table     = lt_selection_table         " Table with ranges structure that contains selections
        selection_table_255 = lt_selection_table_255    " Table with ranges structure which contains selections (RSPAR
      EXCEPTIONS
        not_found           = 1
        no_report           = 2.
    IF sy-subrc EQ 0.
      READ TEXTPOOL iv_repid INTO lt_textpool LANGUAGE sy-langu.
    ENDIF.

    mt_info = get_screen_info( iv_repid ).
    DATA(lv_lastpos) = mc_maxlength - 1.

    LOOP AT lt_selection_table INTO DATA(ls_selection) WHERE selname IN lr_fldhide GROUP BY ( selname = ls_selection-selname )
    ASSIGNING FIELD-SYMBOL(<lfs_selgrp>) .

      lv_text = VALUE #( lt_textpool[ id = 'S' key = <lfs_selgrp>-selname ]-entry OPTIONAL ).
      lv_text = COND #( WHEN lv_text = mc_refval THEN read_dict_text( iv_selname = <lfs_selgrp>-selname )
                        ELSE lv_text ).
      CONDENSE lv_text .
      LOOP AT GROUP <lfs_selgrp> INTO DATA(ls_selgrp) GROUP BY ( kind = ls_selgrp-kind sign = ls_selgrp-sign option = ls_selgrp-option ) ASSIGNING FIELD-SYMBOL(<lfs_selgrp2>).
        DATA(lv_sign) =  COND #( WHEN <lfs_selgrp2>-sign EQ 'I' AND <lfs_selgrp2>-option NE 'NB' THEN '[]'
                                 WHEN <lfs_selgrp2>-sign EQ 'I' AND <lfs_selgrp2>-option EQ 'NB' THEN ']['
                                 WHEN <lfs_selgrp2>-sign EQ 'E' AND <lfs_selgrp2>-option NE 'NB' THEN ']['
                                 WHEN <lfs_selgrp2>-sign EQ 'E' AND <lfs_selgrp2>-option EQ 'NB' THEN '[]'  ).
        IF ( <lfs_selgrp2>-kind EQ 'S' AND lv_sign IS NOT INITIAL ) OR <lfs_selgrp2>-kind EQ 'P'.
          LOOP AT GROUP <lfs_selgrp2> ASSIGNING FIELD-SYMBOL(<lfs_selection>).
            lv_val = |{ lv_val }{ COND #( WHEN lv_val IS NOT INITIAL THEN ',' ) }{ selval( <lfs_selection>  ) }|.
          ENDLOOP.
          lv_texttmp = |{ lv_texttmp }{ COND #( WHEN lv_texttmp IS NOT INITIAL THEN ';' ) }{ lv_sign }{ lv_val }|.
          CLEAR lv_val.
        ELSE.
          CLEAR lv_text.
        ENDIF.
      ENDLOOP.
      IF lv_texttmp IS NOT INITIAL.
        lv_texttmp = |{ lv_text } : { lv_texttmp }|.
        DO.
          IF strlen( lv_texttmp ) > mc_maxlength.
            IF lv_texttmp+lv_lastpos(1) EQ ','.
              lv_text = lv_texttmp(mc_maxlength).
              APPEND lv_text TO rt_text.
              lv_texttmp = lv_texttmp+lv_lastpos(*).
              CONDENSE lv_texttmp.
            ELSE.
              DO.
                lv_offset = lv_lastpos - sy-index.
                IF lv_texttmp+lv_offset(1) EQ ','.
                  lv_offset = lv_offset + 1.
                  lv_text = lv_texttmp(lv_offset).
                  APPEND lv_text TO rt_text.
                  lv_texttmp = lv_texttmp+lv_offset(*).
                  CONDENSE lv_texttmp.
                  EXIT.
                ENDIF.
              ENDDO.
            ENDIF.
          ELSE.
            lv_text = lv_texttmp.
            APPEND lv_text TO rt_text.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
      CLEAR: lv_val, lv_text,lv_texttmp.
    ENDLOOP.
  ENDMETHOD.


  METHOD SELVAL.
    DATA(lv_type) = mt_info[ name = is_selection-selname ]-type.

    rv_text = SWITCH #( is_selection-option WHEN 'EQ' OR '=' OR space OR 'CP' THEN |{ format_out( iv_value = is_selection-low
                                                                                                  iv_type  = lv_type ) }|
                                            WHEN 'NE' OR '<>' OR 'NP' THEN |<>{ format_out( iv_value = is_selection-low
                                                                                            iv_type  = lv_type ) }| ).
    rv_text = SWITCH #( is_selection-option WHEN 'GT' OR  '>' THEN |>{ format_out( iv_value = is_selection-low
                                                                                   iv_type  = lv_type ) }|
                                            WHEN 'GE' OR  '>=' THEN |>={ format_out( iv_value = is_selection-low
                                                                                     iv_type  = lv_type ) }|
                                            WHEN 'LT' OR  '<' THEN |<{ format_out( iv_value = is_selection-low
                                                                                   iv_type  = lv_type ) }|
                                            ELSE rv_text ).

    rv_text = SWITCH #( is_selection-option WHEN 'LE' OR  '<=' THEN |<={ format_out( iv_value = is_selection-low
                                                                                     iv_type  = lv_type ) }|
                                            WHEN 'BT' OR 'NB' THEN |{ format_out( iv_value = is_selection-low
                                                                                  iv_type  = lv_type ) } - { format_out( iv_value = is_selection-high
                                                                                                                         iv_type  = lv_type ) }|
                                            ELSE rv_text ).
  ENDMETHOD.


  METHOD SEND_EMAIL.
    CONSTANTS: lc_raw           TYPE so_obj_tp  VALUE 'RAW',
               lc_att_type_hex  TYPE so_obj_tp  VALUE 'XLS',
               lc_att_type_text TYPE so_obj_tp  VALUE 'EXT'.

    DATA: lv_subject TYPE so_obj_des.
    TRY.
        lv_subject = |{ iv_name }_{ sy-datlo }_{ sy-timlo }|.
        "create persistent send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).
        "create and set document with attachment from internal table with text
        DATA(lo_document) = cl_document_bcs=>create_document( i_type    = lc_raw
                                                              i_text    = it_bodytext
                                                              i_subject = lv_subject ).
        IF lines( it_content_hex ) > 0.
          lo_document->add_attachment(
            EXPORTING
             i_attachment_type    = lc_att_type_hex
             i_attachment_subject = COND #( WHEN iv_flname_hex IS NOT INITIAL THEN |{ iv_flname_hex }_{ sy-datlo }_{ sy-timlo }|
                                            ELSE |{ iv_name }_{ sy-datlo }_{ sy-timlo }| )
             i_att_content_hex    = it_content_hex ).
        ENDIF.
        IF lines( it_content_text ) > 0.
          lo_document->add_attachment(
            EXPORTING
             i_attachment_type    = lc_att_type_text
             i_attachment_subject = COND #( WHEN iv_flname_text IS NOT INITIAL THEN |{ iv_flname_text }_{ sy-datlo }_{ sy-timlo }|
                                            ELSE |{ iv_name }_{ sy-datlo }_{ sy-timlo }| )
             i_att_content_text    = it_content_text ).
        ENDIF.
*       add document to send request
        CALL METHOD lo_send_request->set_document( lo_document ).
* set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.
*        DATA(lo_sender) = cl_sapuser_bcs=>create( sy-uname ).
*        lo_send_request->set_sender( lo_sender ).
** add recipient (e-mail address) -----------------------
        "Send to internal mail
        LOOP AT it_internal_user INTO DATA(ls_user).
          DATA(lo_recipient) = cl_sapuser_bcs=>create( ls_user ).
          lo_send_request->add_recipient( i_recipient = lo_recipient
                                          i_express   = abap_true ).
          CLEAR lo_recipient.
        ENDLOOP.
        "External email
        LOOP AT it_external_email INTO DATA(ls_email).
          DATA(lo_recipient_ext) = cl_cam_address_bcs=>create_internet_address( ls_email ).
          lo_send_request->add_recipient( i_recipient = lo_recipient_ext
                                          i_express   = abap_true ).
          CLEAR lo_recipient_ext.
        ENDLOOP.
* send document ---------------------------------------
        DATA(lv_sent_to_all) = lo_send_request->send( EXPORTING i_with_error_screen = abap_false ).
      CATCH cx_bcs.
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD SET_STYLE.
    IF mo_style IS NOT BOUND.
      mo_style = mo_document->create_simple_element( name = 'Style' parent = mo_styles ).
    ENDIF.
    IF iv_stynm IS SUPPLIED.
      mo_style->set_attribute_ns( name = 'ID' prefix = 'ss' value = iv_stynm ).
    ENDIF.
    IF iv_fornm IS SUPPLIED.
      mo_format = mo_document->create_simple_element( name = iv_fornm parent = mo_style ).
    ENDIF.
    mo_format->set_attribute_ns( name = iv_name prefix = 'ss' value = iv_val ).
  ENDMETHOD.
ENDCLASS.
