class ZCL_CA_UTILITY definition
  public
  final
  create public .

public section.

  types:
    ctt_rsel_info TYPE STANDARD TABLE OF rsel_info WITH DEFAULT KEY .
  types:
    BEGIN OF cty_selval,
        fld     TYPE char8,
        desc    TYPE as4text,
        t_value TYPE sdydo_text_table,
      END OF cty_selval .
  types:
    ctt_selval TYPE STANDARD TABLE OF cty_selval WITH KEY fld .
  types:
    BEGIN OF cty_wtab,
        ucchar(6)    TYPE c,
        char(1)      TYPE c,
        ucix         TYPE i,
        width(4)     TYPE n,
        width_output TYPE dec11_4,
        width_unit   TYPE msehi,
      END OF cty_wtab .
  types:
    ctty_wtab TYPE STANDARD TABLE OF cty_wtab .
  types:
    BEGIN OF cty_upload,
        text TYPE char2000,
      END   OF cty_upload .
  types:
    ctty_upload TYPE STANDARD TABLE OF cty_upload .
  types:
    BEGIN OF cty_txtdata,
        line(5),   "Line count
        xblnr(16), "Reference
        hkont(10), "Gl Account
        wrbtr(16), "Amount in Doc Currency
        pspnr(24), "WBS No. level 3
      END OF cty_txtdata .

  class-data:
    st_tcurx TYPE STANDARD TABLE OF tcurx .
  class-data:
    sr_langu_e TYPE RANGE OF syst-langu .
  constants:
    BEGIN OF cc_type,
        ranges   TYPE ztca_varv-zztype VALUE 'S',
        constant TYPE ztca_varv-zztype VALUE 'P',
        import   TYPE char1            VALUE 'I',
        export   TYPE char1            VALUE 'E',
        langu_zh TYPE spras            VALUE '1',
        langu_th TYPE spras            VALUE '2',
        langu_en TYPE spras            VALUE 'E',
        date     TYPE char1            VALUE 'D',
        format_a TYPE char1            VALUE 'A',
        format_b TYPE char1            VALUE 'B',
        format_c TYPE char1            VALUE 'C',
        ext_csv  TYPE char4            VALUE 'CSV',
        ext_xls  TYPE char4            VALUE 'XLS',
        ext_xlsx TYPE char4            VALUE 'XLSX',
        ext_txt  TYPE char4            VALUE 'TXT',
      END   OF cc_type .
  class-data SW_LOG_COLOURS type BAL_S_COL .
  class-data SV_HANDLER_REGISTERED type CHAR1 .

  class-methods READ_VARIABLE_VALUE
    importing
      value(IV_ZZPROG) type ZTCA_VARV-ZZPROG
      value(IV_ZZNAME) type ZTCA_VARV-ZZNAME
      value(IV_APPEND) type CHAR1 optional
    exporting
      !ET_S_VALUE type STANDARD TABLE
      value(EV_P_VALUE) type ANY
    exceptions
      EX_READ_PARAMETER_NOT_FOUND .
  class-methods READ_LONG_TEXT
    importing
      value(IV_OBJECT) type ANY
      value(IV_ID) type ANY
      value(IV_LANG) type ANY default SY-LANGU
      value(IV_NAME) type ANY
    exporting
      value(ET_LINES) type TLINET
      value(EV_LINE) type ANY
    exceptions
      EX_ID
      EX_LANGUAGE
      EX_NAME
      EX_NOT_FOUND
      EX_OBJECT
      EX_REFERENCE_CHECK
      EX_WRONG_ACCESS_TO_ARCHIVE .
*<--- Start of Insertion By DC_SITTISAK Jun 6, 2018 4:56:07 PM
*---> End of Insertion By DC_SITTISAK Jun 6, 2018 4:56:07 PM
  class-methods READ_ADDRESS
    importing
      value(IV_ADRNR) type ADRNR
      value(IV_LANGU) type SPRAS
      value(IV_LAND1) type LAND1 optional
      value(IV_NATION) type AD_NATION optional
      value(IV_NEXT_SEL) type FLAG optional
      value(IV_DATE) type SY-DATLO default SY-DATLO
      value(IV_TIME) type SY-TIMLO default SY-TIMLO
      value(IV_TIMEZONE) type SY-ZONLO default SY-ZONLO
      value(IV_ONE_NUMBER) type CHAR1 default 'X'
      value(IV_NOTELFAX_HEADING) type CHAR1 default ''
      value(IV_COUNTRY_ALWAYS_DISP) type CHAR1 default ''
      value(IV_DEVTYPE) type TSP0A-PATYPE optional
      value(IV_SAPFONT) type TFO01-TDFAMILY optional
      value(IV_FONTSIZE) type I optional
      value(IV_BOLD) type TDBOLD optional
      value(IV_ITALIC) type TDITALIC optional
      value(IV_WIDTH) type DEC11_4 optional
      value(IV_WIDTH_UNIT) type MSEHI optional
      value(IV_NAME_WIDTH) type I optional
      value(IV_ADDR_WIDTH) type I optional
      value(IV_SEPARATE_CITY1) type FLAG optional
    exporting
      value(EV_NAME_L1) type ANY
      value(EV_NAME_L2) type ANY
      value(EV_NAME_L3) type ANY
      value(EV_NAME_L4) type ANY
      value(EV_ADDR_L1) type ANY
      value(EV_ADDR_L2) type ANY
      value(EV_ADDR_L3) type ANY
      value(EV_ADDR_L4) type ANY
      value(EV_POSTCODE) type ANY
      value(EV_TEL) type ANY
      value(EV_FAX) type ANY
      value(EV_EMAIL) type ANY
    exceptions
      NOT_FOUND .
*<--- Start of Insertion By DC_SITTISAK Aug 2, 2018 7:22:27 PM
*---> End of Insertion By DC_SITTISAK Aug 2, 2018 7:22:27 PM
  class-methods READ_ONE_TIME_ADDRESS
    importing
      value(IV_BUKRS) type BSEG-BUKRS
      value(IV_BELNR) type BSEG-BELNR
      value(IV_GJAHR) type BSEG-GJAHR
      value(IV_LANGU) type SPRAS
      value(IV_LAND1) type LAND1 optional
      value(IV_NATION) type AD_NATION optional
      value(IV_NEXT_SEL) type FLAG optional
      value(IV_DATE) type SY-DATLO default SY-DATLO
      value(IV_TIME) type SY-TIMLO default SY-TIMLO
      value(IV_TIMEZONE) type SY-ZONLO default SY-ZONLO
      value(IV_ONE_NUMBER) type CHAR1 default 'X'
      value(IV_NOTELFAX_HEADING) type CHAR1 default ''
      value(IV_COUNTRY_ALWAYS_DISP) type CHAR1 default ''
      value(IV_DEVTYPE) type TSP0A-PATYPE optional
      value(IV_SAPFONT) type TFO01-TDFAMILY optional
      value(IV_FONTSIZE) type I optional
      value(IV_BOLD) type TDBOLD optional
      value(IV_ITALIC) type TDITALIC optional
      value(IV_WIDTH) type DEC11_4 optional
      value(IV_WIDTH_UNIT) type MSEHI optional
      value(IV_NAME_WIDTH) type I optional
      value(IV_ADDR_WIDTH) type I optional
      value(IV_SEPARATE_CITY1) type FLAG optional
    exporting
      value(EV_NAME_L1) type ANY
      value(EV_NAME_L2) type ANY
      value(EV_NAME_L3) type ANY
      value(EV_NAME_L4) type ANY
      value(EV_ADDR_L1) type ANY
      value(EV_ADDR_L2) type ANY
      value(EV_ADDR_L3) type ANY
      value(EV_ADDR_L4) type ANY
      value(EV_POSTCODE) type ANY
      value(EV_TEL) type ANY
      value(EV_FAX) type ANY
      value(EV_EMAIL) type ANY
      value(EV_TAX) type ANY
    exceptions
      NOT_FOUND .
  class-methods ASSIGN_VALUE
    importing
      value(IV_VALUE) type ANY
    exporting
      value(EV_VALUE) type ANY
    exceptions
      EX_CONVERSION_ERROR .
*<--- Start of Insertion By DC_SITTISAK May 30, 2018 1:59:32 PM
*---> End of Insertion By DC_SITTISAK May 30, 2018 1:59:32 PM
  class-methods IMPORT_FILE_TO_INT_TABLE
    importing
      value(IV_FILENAME) type STRING
      value(IV_BEGIN_COL) type I default 1
      value(IV_BEGIN_ROW) type I default 1
      value(IV_END_COL) type I default 2000
      value(IV_END_ROW) type I default 999999
      value(IV_DATE_FORMAT) type CHAR3 default 'MTJ'     "MM/DD/YYYY
    exporting
      !ET_DATA type STANDARD TABLE
    exceptions
      EX_EXTENSION_FILE_NOT_SUPPORT .
  class-methods IMPORT_CSV_TO_INT_TABLE
    importing
      value(IV_FILENAME) type STRING
      value(IV_BEGIN_ROW) type I default 0
      value(IV_EXCEPTION) type ABAP_BOOL default ABAP_FALSE
      value(IV_ENCODING) type ABAP_ENCODING default '8600'
    exporting
      value(ET_DATA) type STANDARD TABLE
      value(ET_RETURN) type BAPIRET2_T
    exceptions
      EX_FILE_ERROR .
  class-methods CURR_AMOUNT_DISPLAY_TO_SAP
    importing
      value(IV_CURRENCY) type TCURC-WAERS optional
      value(IV_AMOUNT_DISPLAY) type WMTO_S-AMOUNT optional
    exporting
      value(EV_AMOUNT_INTERNAL) type WMTO_S-AMOUNT
    exceptions
      EX_INTERNAL_ERROR .
  class-methods CURR_AMOUNT_SAP_TO_DISPLAY
    importing
      value(IV_CURRENCY) type TCURC-WAERS optional
      value(IV_AMOUNT_INTERNAL) type BSEG-WRBTR optional
    exporting
      value(EV_AMOUNT_DISPLAY) type BSEG-WRBTR
    exceptions
      EX_INTERNAL_ERROR .
  class-methods DATABASE_EXPORT
    importing
      value(IV_DB_ID) type ANY
      value(IV_DB_VALUE) type ANY .
  class-methods DATABASE_IMPORT
    importing
      value(IV_DB_ID) type ANY
      value(IV_CLEAR) type ABAP_BOOL default 'X'
    exporting
      value(EV_VALUE) type ANY .
  class-methods SPELL_AMOUNT
    importing
      value(IV_AMOUNT) type ANY default 0
      value(IV_CURRENCY) type SY-WAERS default SPACE
      value(IV_LANGUAGE) type SY-LANGU default SY-LANGU
      value(IV_TEXTFORMAT) type ZCA_TEXTFORMAT default 'A'
    exporting
      value(EW_ZSPELL) type ZSCA_SPELL
    exceptions
      EX_NOT_FOUND
      EX_TOO_LARGE
      EX_SPELL_ERROR
      EX_INCORRECT_TEXT_FORMAT .
  class-methods SPELL_QUANTITY
    importing
      value(IV_QUANTITY) type ANY default '12345.6789'
      value(IV_UNIT) type MSEHI default 'M'
      value(IV_DECIMAL_PLACE) type I default '4'
      value(IV_LANGUAGE) type SY-LANGU default SY-LANGU
      value(EW_ZSPELL) type ZSCA_SPELL
    exceptions
      EX_NOT_FOUND
      EX_TOO_LARGE
      EX_UNIT_NOT_FOUND .
  class-methods LOG_REFRESH
    for event SAVE_REQUESTED of IF_OS_TRANSACTION .
  class-methods CLASS_CONSTRUCTOR .
  class-methods LOG_CREATE
    importing
      !IV_OBJECT type BALOBJ_D optional
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_EXT_NO type BALNREXT optional
    returning
      value(RV_HANDLE) type BALLOGHNDL .
  class-methods LOG_ADD_MESSAGE
    importing
      !IV_PROBLEMCLASS type BAL_S_MSG-PROBCLASS default '4'
      !IV_HANDLE type BALLOGHNDL
      !IV_CONTEXT type ANY
      !IV_TABNAME type STRUKNAME
    returning
      value(RV_LOG_IS_FULL) type SAP_BOOL .
  class-methods LOG_SHOW
    importing
      !IV_PROTOCOL type BALLOGHNDL
      !IT_FLDCAT type BAL_T_FCAT .
  class-methods LOG_SAVE
    importing
      !IV_LOGHANDLE type BALLOGHNDL
      !IV_UPDATE_TASK type CHAR1 default SPACE
    returning
      value(RV_LOGNO) type BALOGNR .
  class-methods LOG_CLEAR_ALL .
  class-methods LOG_LOAD
    importing
      !IV_HANDLE type BALLOGHNDL
    returning
      value(RV_LOGHANDLE) type BALLOGHNDL .
  class-methods LOG_ADD_MESSAGE_FREE_TEXT
    importing
      !IV_HANDLE type BALLOGHNDL
      !IV_TEXT type C
      !IV_MSGTY type SYMSGTY default 'S'
    returning
      value(RV_LOG_IS_FULL) type SAP_BOOL .
  class-methods LOG_DELETE_MESSAGE
    importing
      !IV_HANDLE type BALLOGHNDL .
  class-methods CONVERT_MATERIAL_UNIT
    importing
      value(IV_MATNR) type MARA-MATNR
      value(IV_IN_UOM) type MARA-MEINS
      value(IV_OUT_UOM) type MARA-MEINS
      value(IV_IN_QTY) type ANY
    exporting
      value(EV_OUT_QTY) type ANY .
  class-methods GENERATE_ABAP_TEMPLATE
    importing
      value(IV_PROGRAM) type PROGRAM optional
      value(IM_PROGRAM) type PROGRAM optional
      value(IT_CODE) type RSWSOURCET optional
      value(IM_CODE_TAB) type RSWSOURCET optional
    exporting
      value(ET_CODE) type RSWSOURCET
      value(EX_CODE_TAB) type RSWSOURCET .
  class-methods MOVE_DATA
    importing
      value(IV_DATA_IN) type ANY
      value(IV_EXCEPTION) type ABAP_BOOL default ABAP_FALSE
    exporting
      value(EV_DATA_OUT) type ANY
    changing
      value(CH_DATAX_IN) type ANY optional
    raising
      resumable(ZCX_CA_MESSAGE) .
  class-methods SET_VALUE_DYNAMIC
    importing
      value(IV_FIELD) type ANY
      value(IV_VALUE) type ANY
      value(IV_SUM) type C optional
      value(IV_SEPAR) type ANY optional
      value(IV_DUP) type ANY default 'X'
    changing
      !CW_LINE type ANY .
  class-methods GET_VALUE_DYNAMIC
    importing
      value(IV_FIELD) type ANY
      value(IW_LINE) type ANY
    changing
      !CV_VALUE type ANY .
  class-methods ROUND_UP_VALUE
    importing
      value(IV_VALUE) type ANY
      value(IV_DECIMAL) type ANY
      value(IV_MODE) type ANY
    exporting
      value(EV_VALUE) type ANY .
  class-methods GET_ABAP_PROXY_MSGID
    importing
      !IO_PROXY type ref to OBJECT optional
    returning
      value(RV_MSGID) type SXMSMGUID
    exceptions
      EX_SYSTEM_FAULT .
  class-methods PREPARE_SELSCR_VALUE
    importing
      !IV_REPID type SYREPID
      !IT_SELPAR type RE_T_SELPAR
    returning
      value(RT_SELVAL) type CTT_SELVAL .
  class-methods DO_PREPARE_SELSCR_VAL
    importing
      !IT_SELECTION_TABLE type PIVB_RSPARAMSL_255_T
      !IT_SELPAR type HRTNM_TAB_RNG_FIELDNAME optional
      !IV_REPID type SYREPID
    returning
      value(RT_SELVAL) type CTT_SELVAL .
  class-methods SET_RANGES
    importing
      !IV_SIGN type CHAR1 default 'I'
      !IV_OPTION type CHAR2 default 'EQ'
      !IV_LOW type CLIKE
      !IV_HIGH type CLIKE optional
      !IV_APPEND type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_RANGE type STANDARD TABLE .
*<--- Start of Insertion By DC_SITTISAK May 17, 2018 11:25:27 AM
  class-methods GET_LOGO_NAME
    importing
      !IV_BUKRS type BUKRS
    returning
      value(RV_LOGONAME) type TDOBNAME
    raising
      ZCX_CA_MESSAGE .
  class-methods IS_LOGO_VALID
    importing
      !IV_LOGONAME type TDOBNAME
    returning
      value(RV_RESULT) type ABAP_BOOL .
*---> End of Insertion By DC_SITTISAK May 17, 2018 11:25:27 AM
*<--- Start of Insertion By DC_SITTISAK May 23, 2018 3:48:02 PM
  class-methods GET_PERSON_NAME
    importing
      !IV_UNAME type SYST-UNAME
    exporting
      !EV_FIRSTNAME type AD_NAMEFIR
      !EV_LASTNAME type AD_NAMELAS
      !EV_FULLNAME type AD_NAMTEXT
    raising
      ZCX_CA_MESSAGE .
  class-methods CONSUME_QUERY_BY_ADBC
    importing
      !IV_SQL type STRING
      !IV_CON_NAME type DBCON_NAME default 'SYSTEMDB@HA3'
    exporting
      !ET_DATA type TABLE
    raising
      ZCX_CA_MESSAGE .
*---> End of Insertion By DC_SITTISAK May 23, 2018 3:48:02 PM
  class-methods CREATE_XLSX_FROM_ITAB
    importing
      !IT_FCAT type LVC_T_FCAT optional          " field catalog for list viewer control
      !IT_SORT type LVC_T_SORT optional          " alv control: table of sort criteria
      !IT_FILT type LVC_T_FILT optional          " alv control: table of filter conditions
      !IW_LAYOUT type LVC_S_LAYO optional      " alv control: layout structure
      !IV_XLSX type ABAP_BOOL optional                " create xlsx file?
    exporting
      !EV_XSTRING type XSTRING                " xstring with our excel file
    changing
      !CT_DATA type STANDARD TABLE .             "internal table
  class-methods MARK_X
    importing
      !IV_STARTPOS type I default 1
      !IW_SOURCE type ANY
    changing
      !CW_TARGET type ANY .
  class-methods CREATE_DYNAMIC_TABLE
    importing
      value(IT_COMP) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    exporting
      value(ED_TABD) type ref to CL_ABAP_TABLEDESCR
      value(ED_TABLE) type ref to DATA
      value(ED_STRD) type ref to CL_ABAP_STRUCTDESCR
      value(ED_STRUCT) type DATA
    raising
      CX_SY_TYPE_CREATION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA st_info TYPE ctt_rsel_info .
    CONSTANTS cc_type_tel TYPE c VALUE 'T' ##NO_TEXT.
    CONSTANTS cc_type_fax TYPE c VALUE 'F' ##NO_TEXT.
    CONSTANTS cc_maxlength TYPE i VALUE 255 ##NO_TEXT.
    CLASS-DATA:
      st_log_handle_tab TYPE TABLE OF balloghndl .

    CLASS-METHODS get_string_width
      IMPORTING
        VALUE(iv_devtype)     TYPE tsp0a-patype
        VALUE(iv_sapfont)     TYPE tfo01-tdfamily
        VALUE(iv_fontsize)    TYPE i
        VALUE(iv_bold)        TYPE tdbold OPTIONAL
        VALUE(iv_italic)      TYPE tditalic OPTIONAL
        VALUE(iv_string)      TYPE any
        VALUE(iv_length_unit) TYPE msehi DEFAULT 'CM'
      EXPORTING
        VALUE(et_wtab)        TYPE ctty_wtab
        VALUE(ev_length)      TYPE dec11_4
        VALUE(ev_length_unit) TYPE msehi
      EXCEPTIONS
        ex_error .
    CLASS-METHODS get_telfax
      IMPORTING
        VALUE(iv_langu)            TYPE spras
        VALUE(iv_type)             TYPE c
        VALUE(iv_number)           TYPE any
        VALUE(iv_ext)              TYPE any
        VALUE(iv_notelfax_heading) TYPE char1 DEFAULT space
      CHANGING
        VALUE(cv_number_text)      TYPE any .
    CLASS-METHODS set_format_address
      IMPORTING
        VALUE(iv_langu)               TYPE spras OPTIONAL
        VALUE(iv_land1)               TYPE land1 OPTIONAL
        VALUE(iv_one_number)          TYPE char1 OPTIONAL
        VALUE(iv_notelfax_heading)    TYPE char1 OPTIONAL
        VALUE(iv_country_always_disp) TYPE char1 OPTIONAL
        VALUE(iw_adrc)                TYPE adrc OPTIONAL
        VALUE(it_adr2)                TYPE tty_adr2 OPTIONAL
        VALUE(it_adr3)                TYPE tty_adr3 OPTIONAL
        VALUE(it_adr6)                TYPE tty_adr6 OPTIONAL
        VALUE(iv_timestamp)           TYPE tstmp OPTIONAL
        VALUE(iv_devtype)             TYPE tsp0a-patype OPTIONAL
        VALUE(iv_sapfont)             TYPE tfo01-tdfamily OPTIONAL
        VALUE(iv_fontsize)            TYPE i OPTIONAL
        VALUE(iv_bold)                TYPE tdbold OPTIONAL
        VALUE(iv_italic)              TYPE tditalic OPTIONAL
        VALUE(iv_width)               TYPE dec11_4 OPTIONAL
        VALUE(iv_width_unit)          TYPE msehi OPTIONAL
        VALUE(iv_name_width)          TYPE i OPTIONAL
        VALUE(iv_addr_width)          TYPE i OPTIONAL
        VALUE(iv_separate_city1)      TYPE flag OPTIONAL
      EXPORTING
        !ev_name_l1                   TYPE any
        !ev_name_l2                   TYPE any
        !ev_name_l3                   TYPE any
        !ev_name_l4                   TYPE any
        !ev_addr_l1                   TYPE any
        !ev_addr_l2                   TYPE any
        !ev_addr_l3                   TYPE any
        !ev_addr_l4                   TYPE any
        !ev_postcode                  TYPE any
        !ev_tel                       TYPE any
        !ev_fax                       TYPE any
        !ev_email                     TYPE any .
    CLASS-METHODS set_format_onetime_address
      IMPORTING
        VALUE(iv_langu)               TYPE spras OPTIONAL
        VALUE(iv_land1)               TYPE land1 OPTIONAL
        VALUE(iv_one_number)          TYPE char1 OPTIONAL
        VALUE(iv_notelfax_heading)    TYPE char1 OPTIONAL
        VALUE(iv_country_always_disp) TYPE char1 OPTIONAL
        VALUE(iw_bsec)                TYPE bsec OPTIONAL
        VALUE(iv_timestamp)           TYPE tstmp OPTIONAL
        VALUE(iv_devtype)             TYPE tsp0a-patype OPTIONAL
        VALUE(iv_sapfont)             TYPE tfo01-tdfamily OPTIONAL
        VALUE(iv_fontsize)            TYPE i OPTIONAL
        VALUE(iv_bold)                TYPE tdbold OPTIONAL
        VALUE(iv_italic)              TYPE tditalic OPTIONAL
        VALUE(iv_width)               TYPE dec11_4 OPTIONAL
        VALUE(iv_width_unit)          TYPE msehi OPTIONAL
        VALUE(iv_name_width)          TYPE i OPTIONAL
        VALUE(iv_addr_width)          TYPE i OPTIONAL
        VALUE(iv_separate_city1)      TYPE flag OPTIONAL
      EXPORTING
        !ev_name_l1                   TYPE any
        !ev_name_l2                   TYPE any
        !ev_name_l3                   TYPE any
        !ev_name_l4                   TYPE any
        !ev_addr_l1                   TYPE any
        !ev_addr_l2                   TYPE any
        !ev_addr_l3                   TYPE any
        !ev_addr_l4                   TYPE any
        !ev_postcode                  TYPE any
        !ev_tel                       TYPE any
        !ev_fax                       TYPE any
        !ev_email                     TYPE any
*<--- Start of Insertion By DC_SITTISAK Aug 2, 2018 7:21:56 PM
        !ev_tax                       TYPE any .
*---> End of Insertion By DC_SITTISAK Aug 2, 2018 7:21:56 PM
    CLASS-METHODS upload_excel_file
      IMPORTING
        VALUE(iv_filename)    TYPE string
        VALUE(iv_begin_col)   TYPE i
        VALUE(iv_begin_row)   TYPE i
        VALUE(iv_end_col)     TYPE i
        VALUE(iv_end_row)     TYPE i
        VALUE(iv_date_format) TYPE char3 DEFAULT 'MTJ'
      EXPORTING
        VALUE(et_data)        TYPE STANDARD TABLE
      RAISING
        zcx_ca_message .
    CLASS-METHODS upload_text_file
      IMPORTING
        VALUE(iv_filename)  TYPE string
        VALUE(iv_begin_row) TYPE i DEFAULT 0
      EXPORTING
        VALUE(et_data)      TYPE STANDARD TABLE
      RAISING
        zcx_ca_message .
    CLASS-METHODS spell_chinese
      IMPORTING
        VALUE(iv_amount) TYPE any
      CHANGING
        VALUE(cw_zspell) TYPE zsca_spell
      EXCEPTIONS
        ex_wrong_money .
    CLASS-METHODS get_screen_info
      IMPORTING
        !iv_repid      TYPE sy-cprog
      RETURNING
        VALUE(rt_info) TYPE ctt_rsel_info .
    CLASS-METHODS selval
      IMPORTING
        VALUE(iw_selection) TYPE rsparamsl_255
      RETURNING
        VALUE(rv_text)      TYPE text255 .
    CLASS-METHODS format_out
      IMPORTING
        VALUE(iv_value) TYPE clike
        VALUE(iv_type)  TYPE clike
      RETURNING
        VALUE(rv_out)   TYPE text255 .
    CLASS-METHODS read_dict_text
      IMPORTING
        !iv_selname    TYPE rsparams-selname
      RETURNING
        VALUE(rv_text) TYPE as4text .
    CLASS-METHODS get_field_info
      IMPORTING
        VALUE(iv_tabname)   TYPE ddobjname
        VALUE(iv_fieldname) TYPE fieldname
      RETURNING
        VALUE(rw_dfies)     TYPE dfies .
    CLASS-METHODS build_fcat
      CHANGING
        !ct_data       TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
ENDCLASS.



CLASS ZCL_CA_UTILITY IMPLEMENTATION.


  METHOD assign_value.
    DATA: lv_mask  TYPE string,
          lv_conv  TYPE rs38l_fnam,
          lv_type  TYPE c,
          lv_value TYPE string.


    DATA: lv_len_in  TYPE i,
          lv_len_out TYPE i.

    DESCRIBE FIELD ev_value EDIT MASK lv_mask
                            TYPE lv_type.
    CASE lv_type.
      WHEN 'h'.
        ev_value = iv_value.
      WHEN OTHERS.
        lv_value = iv_value.
        CASE lv_type.
          WHEN 'P'.
            DESCRIBE FIELD iv_value TYPE lv_type.
            CASE lv_type.
              WHEN 'F' OR 'P'.
                ev_value = iv_value.
              WHEN OTHERS.
                REPLACE ALL OCCURRENCES OF: ',' IN lv_value WITH '',
                                            '%' IN lv_value WITH '',
                                            '"' IN lv_value WITH ''.
                TRY.
                    MOVE lv_value TO ev_value.
                  CATCH cx_sy_conversion_error.
                    CLEAR ev_value.
                    RAISE ex_conversion_error.
                ENDTRY.
            ENDCASE.
          WHEN 'D'.
* Begin of Karn+
            ev_value = lv_value.
            IF ev_value NE space AND
               ev_value NE '00000000'.
              "Checking internal date format YYYYMMDD
              CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                EXPORTING
                  date                      = ev_value
                EXCEPTIONS
                  plausibility_check_failed = 1
                  OTHERS                    = 2.
              IF sy-subrc <> 0.
                "Not internal date format YYYYMMDD
                DATA(lv_value_dot) = lv_value.
                REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN lv_value_dot WITH '.'.
                TRANSLATE lv_value_dot TO UPPER CASE.

                CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                  EXPORTING
                    date_external            = lv_value_dot
*                   ACCEPT_INITIAL_DATE      =
                  IMPORTING
                    date_internal            = ev_value
                  EXCEPTIONS
                    date_external_is_invalid = 1
                    OTHERS                   = 2.
                CHECK sy-subrc NE 0 OR ev_value = space.

                CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
                  EXPORTING
                    input  = lv_value_dot
                  IMPORTING
                    output = ev_value.
                CHECK ev_value = space.

                CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
                  EXPORTING
                    input  = lv_value_dot
                  IMPORTING
                    output = ev_value.
                CHECK ev_value = space.

                "YYYY-MM-DD
                IF strlen( lv_value_dot ) ge 10.
                  IF lv_value_dot+4(1) = '.' AND lv_value_dot+7(1) = '.'.
                    ev_value = |{ lv_value_dot+0(4) }{ lv_value_dot+5(2) }{ lv_value_dot+8(2) }|.
                    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                      EXPORTING
                        date                      = ev_value
                      EXCEPTIONS
                        plausibility_check_failed = 1
                        OTHERS                    = 2.
                    IF sy-subrc = 0.
                      RETURN.
                    ELSE.
                      CLEAR ev_value.
                    ENDIF.
                  ENDIF.
                ENDIF.

*            TRY.
*              CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT(
*                EXPORTING IM_DATEXT    = lv_value_dot
**                         IM_DATFMDES  =
*                IMPORTING EX_DATINT    = ex_value
**                         EX_DATFMUSED =
*                ).
*              CATCH CX_ABAP_DATFM_NO_DATE .        clear ex_value.
*              CATCH CX_ABAP_DATFM_INVALID_DATE .   clear ex_value.
*              CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN . clear ex_value.
*              CATCH CX_ABAP_DATFM_AMBIGUOUS .      clear ex_value.
*            ENDTRY.
*            check ex_value = space.

*            IF ex_value = space.
*              IF lv_value CO '1234567890.,-/\'.
*                "MONTH part is number only
*                "Convert DD.MM.YY or DD.MM.YYYY -> internal date format YYYYMMDD
*                CALL FUNCTION 'KCD_EXCEL_DATE_CONVERT'
*                  EXPORTING
*                    excel_date  = lv_value
**                   DATE_FORMAT = 'TMJ'
*                  IMPORTING
*                    sap_date    = ex_value.
*              ELSE.
*                "MONTH part is JAN, FEB, MAR, APR, ...
*                "Expect date format DD.MMM.YYYY (01.Oct.2016) or DD-MMM-YYYY
*                ex_value = TRANSLATE_DATE( lv_value ).
*              ENDIF.
*            endif.
              ENDIF.
            ENDIF.
* End of Karn+
* Begin of Karn-
*        ev_value = lv_value.
*        IF ev_value NE space AND
*           ev_value NE '00000000'.
*          CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
*            EXPORTING
*              date                      = ev_value
*            EXCEPTIONS
*              plausibility_check_failed = 1
*              OTHERS                    = 2.
*          IF sy-subrc <> 0.
*            CALL FUNCTION 'KCD_EXCEL_DATE_CONVERT'
*              EXPORTING
*                excel_date = lv_value
*              IMPORTING
*                sap_date   = ev_value.
*          ENDIF.
*        ENDIF.
* End of Karn-
          WHEN 'T'.
            REPLACE ALL OCCURRENCES OF ':' IN lv_value WITH ''.
            CASE strlen( lv_value ).
              WHEN 3. " 400   (4:00)    -- HOUR only 1 character & no SECOND part
                ev_value = |0{ lv_value }00|.
              WHEN 5. " 40000 (4:00:00) -- HOUR only 1 character
                ev_value = |0{ lv_value }|.
              WHEN OTHERS.
                ev_value = lv_value.
            ENDCASE.

            IF ev_value NE space AND
               ev_value NE '000000'.
              CALL FUNCTION 'RRSV_TIME_CHECK_PLAUSIBILITY'
                EXPORTING
                  i_time                    = ev_value
                EXCEPTIONS
                  plausibility_check_failed = 1
                  OTHERS                    = 2.
              IF sy-subrc <> 0.
                CALL FUNCTION 'CONVERT_TIME_INPUT'
                  EXPORTING
                    input                     = lv_value
                  IMPORTING
                    output                    = ev_value
                  EXCEPTIONS
                    plausibility_check_failed = 1
                    wrong_format_in_input     = 2
                    OTHERS                    = 3.
                IF sy-subrc NE 0.
                  TRY.
                      MOVE lv_value TO ev_value.
                    CATCH cx_sy_conversion_error.
                      CLEAR ev_value.
                      RAISE ex_conversion_error.
                  ENDTRY.
                ENDIF.
              ENDIF.
            ENDIF.
          WHEN 'N'.       "NEW!!!!'
            IF lv_value CA '0123456789 '.
              ev_value = lv_value.
            ELSE.
              CLEAR ev_value.
              RAISE ex_conversion_error.
            ENDIF.
          WHEN 'F'.       "NEW!!!!
            "CHAR -> FLTP .... *** THERE IS A ROUNDING ISSUE ***
            CALL FUNCTION 'CHAR_FLTP_CONVERSION'
              EXPORTING
                string             = lv_value
              IMPORTING
                flstr              = ev_value
              EXCEPTIONS
                exponent_too_big   = 1
                exponent_too_small = 2
                string_not_fltp    = 3
                too_many_decim     = 4
                OTHERS             = 5.
            IF sy-subrc <> 0.
              ev_value = lv_value.
            ENDIF.
          WHEN OTHERS.
            IF lv_mask IS NOT INITIAL.
              CONCATENATE 'CONVERSION_EXIT_' lv_mask+2 '_INPUT' INTO lv_conv.
              TRY.
*                  lv_len_in = strlen( lv_value ).
*                  DESCRIBE FIELD ev_value  LENGTH lv_len_out IN CHARACTER MODE.
*
*                  IF lv_len_in > lv_len_out.
*                    RAISE ex_conversion_error.
*                  ENDIF.

                  conversion_input lv_conv lv_value ev_value.
                  IF sy-subrc NE 0.
                    RAISE ex_conversion_error.
                  ENDIF.
                CATCH cx_root .

                  CLEAR ev_value.
                  RAISE ex_conversion_error.
              ENDTRY.
            ELSE.
              ev_value = lv_value.
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.


  METHOD build_fcat.
    TRY.
        cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = DATA(lo_salv)    " Basis Class Simple ALV Tables
          CHANGING
            t_table        = ct_data
        ).

        cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          EXPORTING
            r_columns      = lo_salv->get_columns( )    " ALV Filter
            r_aggregations = lo_salv->get_aggregations( )    " ALV Aggregations
          RECEIVING
            t_fieldcatalog = rt_fcat    " Field Catalog for List Viewer Control
        ).
      CATCH cx_salv_msg.
        "handle exception
    ENDTRY.
*      CATCH cx_salv_msg.    "
  ENDMETHOD.


  METHOD class_constructor.
    sw_log_colours-probclass1 = 6. "red
    sw_log_colours-probclass2 = 6. "red
    sw_log_colours-probclass4 = 4. "blue
  ENDMETHOD.


  METHOD consume_query_by_adbc.
    TRY.
****Create the SQL Connection and pass in the DBCON ID to state which Database Connection will be used
        DATA(lo_sql) = NEW cl_sql_statement( con_ref = cl_sql_connection=>get_connection( iv_con_name ) ).
****Execute a query, passing in the query string and receiving a result set object
        DATA(lo_result) = lo_sql->execute_query( iv_sql ).
****All data (parameters in, results sets back) is done via data references
        DATA(ld_data) = REF #( et_data ).
****Get the result data set back into our ABAP internal table
        lo_result->set_param_table( ld_data ).
        lo_result->next_package( ).
        lo_result->close( ).
      CATCH cx_sql_exception cx_parameter_invalid.
        RAISE EXCEPTION TYPE zcx_ca_message
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ENDMETHOD.


  METHOD convert_material_unit.
    CALL FUNCTION 'OIB_MATERIAL_UNIT_CONVERSION'
      EXPORTING
        iv_matnr            = iv_matnr
        iv_inuom            = iv_in_uom
        iv_outuom           = iv_out_uom
        iv_quantity         = iv_in_qty
      IMPORTING
        ev_quantity         = ev_out_qty
      EXCEPTIONS
        error_material_read = 1
        conversion_failed   = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      CLEAR ev_out_qty.
    ENDIF.
  ENDMETHOD.


  METHOD create_dynamic_table.
    CHECK lines( it_comp ) GT 0.
    DATA: ld_outtab TYPE REF TO data,
          ld_outstr TYPE REF TO data,
          lo_tabd   TYPE REF TO cl_abap_tabledescr,
          lo_strd   TYPE REF TO cl_abap_structdescr.

    "Create dynamic table descriptor
    try.
       lo_strd = cl_abap_structdescr=>create( p_components = it_comp
                                              p_strict     = abap_false ).
      catch cx_sy_struct_creation into data(lo_cx_str).
        raise exception lo_cx_str.
*        raise exception type CX_SY_TYPE_CREATION.
    endtry.

    "Create dynamic structure descriptor
    try.
*        lo_tabd = cl_abap_tabledescr=>create( p_line_type  = cl_abap_structdescr=>create( it_comp )
*                                              p_table_kind = cl_abap_tabledescr=>tablekind_std
*                                              p_unique     = abap_false ).
        lo_tabd = cl_abap_tabledescr=>create( lo_strd ).
      catch CX_SY_TABLE_CREATION into data(lo_cx_tab).
        raise exception lo_cx_tab.
*        raise exception type CX_SY_TYPE_CREATION.
    endtry.

    "Create dynamic table / structure
    CREATE DATA ld_outtab TYPE HANDLE lo_tabd.
    CREATE DATA ld_outstr TYPE HANDLE lo_strd.

    "Table
    if ed_tabd is supplied.
      ed_tabd   = lo_tabd.
    endif.
    if ed_table is supplied.
      ed_table  = ld_outtab.
    endif.

    "Structure
    if ed_strd is supplied.
      ed_strd   = lo_strd.
    endif.
    if ed_struct is supplied.
      ed_struct = ld_outstr.
    endif.
  ENDMETHOD.


  METHOD create_xlsx_from_itab.
    "Prepare field catalog
    DATA(lt_fcat) = COND #( WHEN it_fcat IS SUPPLIED THEN it_fcat ELSE build_fcat( CHANGING ct_data = ct_data ) ).

    DATA(lv_version) = SWITCH #( cl_salv_bs_a_xml_base=>get_version( ) WHEN if_salv_bs_xml=>version_25 THEN if_salv_bs_xml=>version_25
                                                                       WHEN if_salv_bs_xml=>version_26 THEN if_salv_bs_xml=>version_26
                                                                       WHEN if_salv_bs_xml=>version_27 THEN if_salv_bs_xml=>version_27 ).
    CHECK lv_version IS NOT INITIAL.
    DATA(lo_result) = cl_salv_ex_util=>factory_result_data_table(
                        EXPORTING
*                          t_selected_rows        =     " ALV Control: Table Rows
*                          t_selected_columns     =     " ALV Control: Table with Rows of Type LVC_S_COL
*                          t_selected_cells       =     " ALV control: Table with cell descriptions
                          r_data                 = REF #( ct_data )    " Data table
                          s_layout               = iw_layout    " ALV Control: Layout Structure
                          t_fieldcatalog         = lt_fcat    " Field Catalog for List Viewer Control
                          t_sort                 = it_sort    " ALV Control: Table of Sort Criteria
                          t_filter               = it_filt    " ALV Control: Table of Filter Conditions
*                          t_hyperlinks           =     " ALV Control: Hyperlinks
*                          s_current_cell         =     " ALV Control: Cell Description
*                          hyperlink_entry_column =
*                          dropdown_entry_column  =
*                          t_dropdown_values      =     " ALV Control: Dropdown List Boxes
*                          r_top_of_list          =     " Set and Get Design Object Content
*                          r_end_of_list          =     " Set and Get Design Object Content
    ).
    "if we flag i_XLSX then we'll create XLSX if not then MHTML excel file
    DATA(lv_file_type) = COND #( WHEN iv_xlsx EQ abap_true THEN if_salv_bs_xml=>c_type_xlsx
                                 ELSE if_salv_bs_xml=>c_type_mhtml ).

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_version   = lv_version    " XML Version to be Selected
        r_result_data = lo_result
        xml_type      = lv_file_type    " XML Type as SALV Constant
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui    " Constant
      IMPORTING
        xml           = ev_xstring
*        filename      =
*        mimetype      =
*        t_msg         =     " Messages
    ).
  ENDMETHOD.


  METHOD curr_amount_display_to_sap.
    DATA: lv_shift(2) TYPE p.
    CLEAR: lv_shift.

    READ TABLE st_tcurx INTO DATA(lw_tcurx)
                        WITH KEY currkey = iv_currency.
    CASE sy-subrc.
      WHEN 0.
        lv_shift = lw_tcurx-currdec - 2.
      WHEN OTHERS.
        SELECT SINGLE * INTO lw_tcurx
          FROM tcurx
         WHERE currkey = iv_currency.
        CASE sy-subrc.
          WHEN 0.
            lv_shift = lw_tcurx-currdec - 2.
            APPEND lw_tcurx TO st_tcurx.
          WHEN 4.
            CLEAR: lw_tcurx.
            lw_tcurx-currkey = iv_currency.
            lw_tcurx-currdec = 2.
            APPEND lw_tcurx TO st_tcurx.
          WHEN OTHERS.
            RAISE ex_internal_error.
*   Fehler beim Lesen der Datenbanktabelle &1
        ENDCASE.
    ENDCASE.

    ev_amount_internal = iv_amount_display * ipow( base = 10
                                                   exp  = lv_shift ).
  ENDMETHOD.


  METHOD curr_amount_sap_to_display.
    DATA: lv_shift(2) TYPE p.
    CLEAR: lv_shift.

    READ TABLE st_tcurx INTO DATA(lw_tcurx)
                        WITH KEY currkey = iv_currency.
    CASE sy-subrc.
      WHEN 0.
        lv_shift = 2 - lw_tcurx-currdec.
      WHEN OTHERS.
        SELECT SINGLE * INTO lw_tcurx
          FROM tcurx
         WHERE currkey EQ iv_currency.
        CASE sy-subrc.
          WHEN 0.
            lv_shift = 2 - lw_tcurx-currdec.
            APPEND lw_tcurx TO st_tcurx.
          WHEN 4.
            CLEAR: lw_tcurx.
            lw_tcurx-currkey = iv_currency.
            lw_tcurx-currdec = 2.
            APPEND lw_tcurx TO st_tcurx.
          WHEN OTHERS.
            RAISE ex_internal_error.
*   Fehler beim Lesen der Datenbanktabelle &1
        ENDCASE.
    ENDCASE.

    ev_amount_display = iv_amount_internal * ipow( base = 10
                                                   exp  = lv_shift ).
  ENDMETHOD.


  METHOD database_export.
    CHECK iv_db_id    IS NOT INITIAL.
    CHECK iv_db_value IS NOT INITIAL.

    DATA: lv_id TYPE char30.
    lv_id = iv_db_id.
    EXPORT v_value = iv_db_value TO DATABASE demo_indx_table(zg) ID lv_id.
  ENDMETHOD.


  METHOD database_import.
    DATA: lv_id TYPE char30.
    lv_id = iv_db_id.
    IMPORT v_value TO ev_value FROM DATABASE demo_indx_table(zg) ID lv_id.

    CHECK iv_clear IS NOT INITIAL.

    DELETE FROM DATABASE demo_indx_table(zg) ID lv_id.
  ENDMETHOD.


  METHOD do_prepare_selscr_val.
    DATA:
      lt_textpool TYPE textpool_table,
      lv_text     TYPE text255,
      lv_val      TYPE string,
      lv_texttmp  TYPE string,
      lv_offset   TYPE i.
    READ TEXTPOOL iv_repid INTO lt_textpool LANGUAGE sy-langu.

    st_info = get_screen_info( iv_repid ).
    DATA(lv_lastpos) = cc_maxlength - 1.

    LOOP AT it_selection_table INTO DATA(ls_selection) WHERE selname IN it_selpar GROUP BY ( selname = ls_selection-selname )
    ASSIGNING FIELD-SYMBOL(<lf_selgrp>) .

      APPEND INITIAL LINE TO rt_selval ASSIGNING FIELD-SYMBOL(<lf_selval>).
      <lf_selval>-fld = <lf_selgrp>-selname.
      <lf_selval>-desc = VALUE #( lt_textpool[ id = 'S' key = <lf_selgrp>-selname ]-entry OPTIONAL ).
      <lf_selval>-desc = COND #( WHEN <lf_selval>-desc = 'D' THEN read_dict_text( iv_selname = <lf_selgrp>-selname )
                        ELSE <lf_selval>-desc ).
      CONDENSE <lf_selval>-desc .

      LOOP AT GROUP <lf_selgrp> INTO DATA(ls_selgrp) GROUP BY ( kind = ls_selgrp-kind sign = ls_selgrp-sign option = ls_selgrp-option ) ASSIGNING FIELD-SYMBOL(<lf_selgrp2>).
        DATA(lv_sign) =  COND #( WHEN <lf_selgrp2>-sign EQ 'I' AND <lf_selgrp2>-option EQ 'NB' THEN ']['
                                 WHEN <lf_selgrp2>-sign EQ 'I' AND <lf_selgrp2>-option NE 'NB' THEN '[]'
                                 WHEN <lf_selgrp2>-sign EQ 'E' AND <lf_selgrp2>-option EQ 'NB' THEN '[]'
                                 WHEN <lf_selgrp2>-sign EQ 'E' AND <lf_selgrp2>-option NE 'NB' THEN '][' ).
        IF ( <lf_selgrp2>-kind EQ 'S' AND lv_sign IS NOT INITIAL ) OR <lf_selgrp2>-kind EQ 'P'.
          LOOP AT GROUP <lf_selgrp2> ASSIGNING FIELD-SYMBOL(<lf_selection>).
            lv_val = |{ lv_val }{ COND #( WHEN lv_val IS NOT INITIAL THEN ', ' ) }{ selval( <lf_selection>  ) }|.
          ENDLOOP.
          lv_texttmp = |{ lv_texttmp }{ COND #( WHEN lv_texttmp IS NOT INITIAL THEN ';' ) }{ lv_sign }{ lv_val }|.
          CLEAR lv_val.
        ENDIF.
      ENDLOOP.
      IF lv_texttmp IS NOT INITIAL.
        DO.
          IF strlen( lv_texttmp ) > cc_maxlength.
            IF lv_texttmp+lv_lastpos(1) EQ ','.
              lv_text = lv_texttmp(cc_maxlength).
              APPEND lv_text TO <lf_selval>-t_value.
              lv_texttmp = lv_texttmp+lv_lastpos(*).
              CONDENSE lv_texttmp.
            ELSE.
              DO.
                lv_offset = lv_lastpos - sy-index.
                IF lv_texttmp+lv_offset(1) EQ ','.
                  lv_offset = lv_offset + 1.
                  lv_text = lv_texttmp(lv_offset).
                  APPEND lv_text TO <lf_selval>-t_value.
                  lv_texttmp = lv_texttmp+lv_offset(*).
                  CONDENSE lv_texttmp.
                  EXIT.
                ENDIF.
              ENDDO.
            ENDIF.
          ELSE.
            lv_text = lv_texttmp.
            APPEND lv_text TO <lf_selval>-t_value.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
      CLEAR: lv_val, lv_text,lv_texttmp.
    ENDLOOP.
  ENDMETHOD.


  METHOD format_out.
    rv_out = COND #( WHEN iv_type = 'D' THEN |{ CONV datum( iv_value ) DATE =  USER }|
                     WHEN iv_type = 'T' THEN |{ CONV uzeit( iv_value ) TIME =  USER }|
                     ELSE iv_value ).
  ENDMETHOD.


  METHOD generate_abap_template.
    SELECT SINGLE devclass
      INTO @DATA(lv_devclass)
      FROM tadir
     WHERE obj_name EQ @iv_program.
    CHECK lv_devclass NE '$TMP'.

    SELECT SINGLE subc
      INTO @DATA(lv_subc)
      FROM trdir
     WHERE name = @iv_program
       AND subc = 1.
    CHECK sy-subrc = 0.
    READ TABLE it_code INTO DATA(lw_code) INDEX 2.
    IF sy-subrc = 0 AND NOT lw_code CP '* Project'.
      et_code = VALUE rswsourcet(
      ( |{ TEXT-l01 }| )
      ( |* Project      : TMG M-Change Phase 1| )
      ( |* RICEF Name   : | )
      ( |* Program Name : { iv_program }| )
      ( |* Description  : | )
      ( |* Created By   : { sy-uname+3 }| )
      ( |* Created Date : { sy-datum DATE = USER }| )
      ( |* TR No.       : | )
      ( |{ TEXT-l01 }| )
      ( |* Modification History                                                 *| )
      ( |{ TEXT-l01 }| )
      ( |* Change ID.  Name             Date        Request No.   Defect/CR No.| )
      ( |* Description:| )
      ( |{ TEXT-l01 }| )
      ( |* CHxxx       XXXXXXX          DD/MM/YYYY  ERDKxxxxxx    IS:NNNNNN| )
      ( |* Description:| )
      ( |{ TEXT-l01 }| )
      ( |REPORT { iv_program } NO STANDARD PAGE HEADING| )
      ( |  MESSAGE-ID Z{ iv_program+3(2) }00.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* INCLUDE PROGRAMS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* TABLES| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* TYPE-POOLS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* TYPES| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* INTERNAL TABLE| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* WORKING AREA| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* VARIABLE| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* CONSTANTS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* RANGES| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* MACROS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* FIELD-GROUPS/FIELD-SYMBOLS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* PARAMETERS & SELECT-OPTIONS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* OBJECT CLASS| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* CLASS LOCAL DEFINITION| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* CLASS LOCAL IMPLEMENTATION| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* INITIALIZATION| )
      ( |{ TEXT-l01 }| )
      ( |INITIALIZATION.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* AT SELECTION-SCREEN| )
      ( |{ TEXT-l01 }| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* AT USER-COMMAND| )
      ( |{ TEXT-l01 }| )
      ( |AT USER-COMMAND.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* AT LINE-SELECTION| )
      ( |{ TEXT-l01 }| )
      ( |AT LINE-SELECTION.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* TOP-OF–PAGE| )
      ( |{ TEXT-l01 }| )
      ( |TOP-OF-PAGE.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* START-OF-SELECTION| )
      ( |{ TEXT-l01 }| )
      ( |START-OF-SELECTION.| )
      ( || )
      ( |{ TEXT-l01 }| )
      ( |* END-OF-SELECTION| )
      ( |{ TEXT-l01 }| )
      ( |END-OF-SELECTION.| ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_abap_proxy_msgid.
    "Use this method for retrieve ABAP proxy message ID.
    DATA: lo_msg_protocal TYPE REF TO if_wsprotocol_message_id,
          lo_protocol     TYPE REF TO if_wsprotocol.

    IF io_proxy IS SUPPLIED.
*   Get Protocol Class of parameter: Message_ID
      CALL METHOD io_proxy->('GET_PROTOCOL')
        EXPORTING
          protocol_name = if_wsprotocol=>message_id
        RECEIVING
          protocol      = lo_protocol.
      lo_msg_protocal ?= lo_protocol.
    ELSE.
      TRY.
          DATA(lo_server_context) = cl_proxy_access=>get_server_context( ).
          lo_msg_protocal ?= lo_server_context->get_protocol( if_wsprotocol=>message_id ).
        CATCH cx_ai_system_fault.
          RAISE ex_system_fault.
      ENDTRY.
    ENDIF.
    rv_msgid = lo_msg_protocal->get_message_id( ).
  ENDMETHOD.


  METHOD get_field_info.
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
      rw_dfies = VALUE #( lt_dfies_tab[ 1 ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD get_logo_name.
*<--- Start of Insertion By DC_SITTISAK May 17, 2018 11:29:14 AM
    "Get Logo name from maintain table ZTCA_LOGO
    SELECT SINGLE logo_name
      FROM ztca_logo
      INTO @DATA(lv_logoname)
     WHERE bukrs = @iv_bukrs.

    "Check validity
    IF NOT is_logo_valid( lv_logoname ).
      RAISE EXCEPTION TYPE zcx_ca_message
        MESSAGE ID 'ZCA00' TYPE 'E' NUMBER '104'
        WITH lv_logoname.
    ELSE.
      rv_logoname = lv_logoname.
    ENDIF.
*---> End of Insertion By DC_SITTISAK May 17, 2018 11:29:14 AM
  ENDMETHOD.


  METHOD get_person_name.
*<--- Start of Insertion By DC_SITTISAK May 23, 2018 3:49:39 PM
    SELECT SINGLE name_first, name_last, name_text
      INTO ( @ev_firstname, @ev_lastname, @ev_fullname )
      FROM usr21 AS a INNER JOIN adrp AS b
        ON a~persnumber = b~persnumber
     WHERE a~bname = @iv_uname.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ca_message
        MESSAGE ID 'ZCA00' TYPE 'E' NUMBER '071'.
    ENDIF.
*---> End of Insertion By DC_SITTISAK May 23, 2018 3:49:39 PM
  ENDMETHOD.


  METHOD get_screen_info.
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


  METHOD get_string_width.
    DATA: lv_devtype     TYPE tsp0a-patype,
          lv_sapfont     TYPE tfo01-tdfamily,
          lv_fontsize    TYPE tfo03-tdfontsize,
          lv_bold        TYPE tdbold,
          lv_italic      TYPE tditalic,
          lv_string      TYPE string,
          lv_length_unit TYPE msehi,
          lw_fh          TYPE itcfh,
          lv_cascading   TYPE c,
          lv_msg(80)     TYPE c,
          lv_length_in   TYPE dec11_4,
          lv_length      TYPE dec11_4,
          lv_length_sum  TYPE dec11_4.

    lv_devtype     = iv_devtype.
    lv_sapfont     = iv_sapfont.
    lv_fontsize    = iv_fontsize * 10.
    lv_bold        = iv_bold.
    lv_italic      = iv_italic.
    lv_string      = iv_string.
    lv_length_unit = iv_length_unit.
*----------------------------------------------------------------------
* Similar to standard program RSPOCFTEST1
    PERFORM ucfmet_init_fontmetric IN PROGRAM saplstxf
      USING lv_devtype
            lv_sapfont
            lv_fontsize
            lv_bold
            lv_italic
            lw_fh
            lv_msg
            lv_cascading.
    IF sy-subrc <> 0.
      RAISE ex_error.
    ENDIF.
*----------------------------------------------------------------------
* Logic from Subroutine CALC_WIDTH in RSPOCFTEST1
    DATA: lv_len            TYPE i,
          lv_c              TYPE c,
          lv_ofs            TYPE i,
          lv_ucix           TYPE i,
          lv_ucchar         TYPE scpuchar,
          lv_width          TYPE i,
          lv_x2(2)          TYPE x,
          lv_stringwidth(6) TYPE n.

    DATA: lt_wtab TYPE ctty_wtab,
          lw_wtab TYPE cty_wtab.

    CONSTANTS: lc_nowidth(4) TYPE n VALUE '9999'.

    lv_len    = strlen( lv_string ). "discard blanks at end
    lv_ucchar = '000000'.
    lv_ofs    = 0.

    DO lv_len TIMES.
      lv_c = lv_string+lv_ofs(1).

      CALL FUNCTION 'SSFCOMP_GET_UCCHAR_IN_UC'
        EXPORTING
          i_char        = lv_c
        IMPORTING
          e_ucix        = lv_ucix
        EXCEPTIONS
          convert_error = 1.
      IF sy-subrc <> 0.
        RAISE ex_error.
      ENDIF.

      lv_x2 = lv_ucix.
      WRITE lv_x2 TO lv_ucchar+2(4).

      PERFORM ucfmet_charwidth_ucix IN PROGRAM saplstxf
        USING lv_ucix lv_width.
      IF sy-subrc <> 0.
        lv_width = lc_nowidth.
        CONTINUE.
      ENDIF.
*   fill wtab
      CLEAR lw_wtab.
      lw_wtab-char = lv_c.
      WRITE lv_ucchar TO lw_wtab-ucchar.
      lw_wtab-ucix  = lv_ucix.
      lw_wtab-width = lv_width.
      APPEND lw_wtab TO lt_wtab.
      ADD 1 TO lv_ofs.
    ENDDO.

    CLEAR lv_length_sum.
    LOOP AT lt_wtab ASSIGNING FIELD-SYMBOL(<fs_wtab>).
      IF <fs_wtab>-width NE lc_nowidth.
        ADD <fs_wtab>-width TO lv_stringwidth.

        CLEAR: lv_length_in, lv_length.
        "Length (1/1440 Inch -> Inch)
        lv_length_in = <fs_wtab>-width / 1440.

        "Length (Inch -> Desired Unit)
        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input                = lv_length_in
            unit_in              = 'IN'
            unit_out             = lv_length_unit
          IMPORTING
            output               = lv_length
          EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9
            OTHERS               = 10.
        IF sy-subrc = 0.
          <fs_wtab>-width_output = lv_length.
          <fs_wtab>-width_unit   = lv_length_unit.

          ADD lv_length TO lv_length_sum.
        ENDIF.
      ENDIF.
    ENDLOOP.
*----------------------------------------------------------------------
* Returning values
    et_wtab        = lt_wtab.
    ev_length      = lv_length_sum.
    ev_length_unit = lv_length_unit.
*----------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_telfax.
    DATA: lv_string     TYPE string,
          lv_nbr_prefix TYPE string,
          lv_ext_prefix TYPE string,
          lv_tel        TYPE char10,
          lv_fax        TYPE char10,
          lv_ext        TYPE char10.

    CLEAR lv_string.

    CASE iv_langu.
      WHEN cc_type-langu_zh.
        lv_tel = TEXT-zh1.
        lv_fax = TEXT-zh2.
        lv_ext = space.
      WHEN cc_type-langu_th.
        lv_tel = TEXT-th1.
        lv_fax = TEXT-th2.
        lv_ext = TEXT-th3.
      WHEN OTHERS.
        lv_tel = TEXT-en1.
        lv_fax = TEXT-en2.
        lv_ext = TEXT-en3.
    ENDCASE.

    IF cv_number_text IS INITIAL.
      IF iv_type = cc_type_fax.
        lv_nbr_prefix = lv_fax.
      ELSE.
        lv_nbr_prefix = lv_tel.
      ENDIF.
    ELSE.
      CLEAR lv_nbr_prefix.
    ENDIF.
    lv_ext_prefix = lv_ext.

    IF iv_notelfax_heading IS INITIAL.
      IF NOT iv_ext IS INITIAL.
        CONCATENATE lv_nbr_prefix iv_number lv_ext_prefix iv_ext
               INTO lv_string SEPARATED BY space.
      ELSE.
        CONCATENATE lv_nbr_prefix iv_number
               INTO lv_string SEPARATED BY space.
      ENDIF.
    ELSE.
      IF NOT iv_ext IS INITIAL.
        CONCATENATE iv_number lv_ext_prefix iv_ext
               INTO lv_string SEPARATED BY space.
      ELSE.
        lv_string = iv_number.
      ENDIF.
    ENDIF.

    IF cv_number_text IS INITIAL.
      cv_number_text = lv_string.
    ELSE.
      CONCATENATE cv_number_text lv_string
             INTO cv_number_text SEPARATED BY ', '.
    ENDIF.
  ENDMETHOD.


  METHOD get_value_dynamic.
    ASSIGN COMPONENT iv_field OF STRUCTURE iw_line TO FIELD-SYMBOL(<lf_field>).
    IF <lf_field> IS ASSIGNED.
      cv_value = <lf_field>.
    ELSE.
      CLEAR cv_value.
    ENDIF.
  ENDMETHOD.


  METHOD import_csv_to_int_table.
    DATA: lo_csv      TYPE REF TO cl_rsda_csv_converter,
          lo_line     TYPE REF TO data,
          lo_table    TYPE REF TO data,
          lt_text     TYPE STANDARD TABLE OF string,
          lw_text     TYPE string,
          lt_fldcat   TYPE lvc_t_fcat,
          lw_fldcat   TYPE lvc_s_fcat,
          lv_row      TYPE i,
          lv_encoding TYPE ABAP_ENCODING.

    FIELD-SYMBOLS:
      <lf_table> TYPE STANDARD TABLE,
      <lf_line>  TYPE any,
      <lf_text>  TYPE any.

    if iv_encoding is not initial.
      lv_encoding = iv_encoding.
    else.
      lv_encoding = '8600'.
    endif.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = iv_filename
        codepage                = lv_encoding
      CHANGING
        data_tab                = lt_text
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING ex_file_error.
    ELSE.
*   Instantiate the CSV object
      lo_csv = cl_rsda_csv_converter=>create( ).

*   Create line of data tab
      CREATE DATA lo_line LIKE LINE OF et_data.
      ASSIGN lo_line->* TO <lf_line>.

      DATA(lt_component) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <lf_line> ) )->components.

      LOOP AT lt_component INTO DATA(lw_component).
        CLEAR lw_fldcat.
        lw_fldcat-fieldname = lw_component-name.
        lw_fldcat-datatype  = 'CHAR'.
        IF lw_component-type_kind = 'P'.
          lw_fldcat-outputlen = ( 2 * lw_component-length ) + 3.
        ELSE.
          lw_fldcat-outputlen = lw_component-length.
        ENDIF.
*        lw_fldcat-outputlen = lw_component-length.
        APPEND lw_fldcat TO lt_fldcat.
      ENDLOOP.
* Create dynamic internal table and assign to FS
      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog = lt_fldcat
        IMPORTING
          ep_table        = lo_table.

      ASSIGN lo_table->* TO <lf_table>.
* Create dynamic work area and assign to FS
      CREATE DATA lo_line LIKE LINE OF <lf_table>.
      ASSIGN lo_line->* TO <lf_text>.

*   Process records
      LOOP AT lt_text INTO lw_text FROM iv_begin_row.
        CALL METHOD lo_csv->csv_to_structure
          EXPORTING
            i_data   = lw_text
          IMPORTING
            e_s_data = <lf_text>.

        TRY.
            ADD 1 TO lv_row.
            move_data( EXPORTING iv_data_in   = <lf_text>
                                 iv_exception = iv_exception
                       IMPORTING ev_data_out  = <lf_line> ).
          CATCH BEFORE UNWIND zcx_ca_message INTO DATA(lo_msg).
            IF iv_exception = abap_true.
              APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<lf_return>).
              <lf_return>-type       = lo_msg->if_t100_dyn_msg~msgty.
              <lf_return>-id         = lo_msg->if_t100_message~t100key-msgid.
              <lf_return>-number     = lo_msg->if_t100_message~t100key-msgno.
              <lf_return>-message    = lo_msg->get_text( ).
              <lf_return>-message_v1 = lo_msg->if_t100_dyn_msg~msgv1.
              <lf_return>-message_v2 = lo_msg->if_t100_dyn_msg~msgv2.
              <lf_return>-message_v3 = lo_msg->if_t100_dyn_msg~msgv3.
              <lf_return>-message_v4 = lo_msg->if_t100_dyn_msg~msgv4.
              <lf_return>-row        = lv_row.
*            message id lo_msg->IF_T100_MESSAGE~T100KEY-msgid
*                    type lo_msg->IF_T100_DYN_MSG~MSGTY
*                  number lo_msg->IF_T100_MESSAGE~T100KEY-msgno
*                    with lo_msg->IF_T100_DYN_MSG~MSGV1
*                         lo_msg->IF_T100_DYN_MSG~MSGV2
*                         lo_msg->IF_T100_DYN_MSG~MSGV3
*                         lo_msg->IF_T100_DYN_MSG~MSGV4
*                 raising ex_file_error.
              IF lo_msg->is_resumable = abap_true.
                RESUME.
              ENDIF.
            ENDIF.
        ENDTRY.
        IF <lf_line> IS NOT INITIAL.
          APPEND <lf_line> TO et_data.
          CLEAR <lf_line>.
        ENDIF.
      ENDLOOP.

      IF iv_exception = abap_true.
        IF lines( et_return ) GT 0.
          "Do not raise EX_FILE_ERROR -> ET_DATA, ET_RETURN will be blank outside
          "More detail can be found in ET_RETURN
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD import_file_to_int_table.

    DATA: lv_ext TYPE string.

    CALL FUNCTION 'CRM_IC_WZ_SPLIT_FILE_EXTENSION'
      EXPORTING
        iv_filename_with_ext = iv_filename
      IMPORTING
        ev_extension         = lv_ext.

    IF lv_ext IS NOT INITIAL.
      TRANSLATE lv_ext TO UPPER CASE.
*      CASE lv_ext(4).
      CASE lv_ext.
        WHEN cc_type-ext_xls OR cc_type-ext_xlsx OR cc_type-ext_csv.
          TRY.
              upload_excel_file( EXPORTING iv_filename  = iv_filename
                                           iv_begin_col = iv_begin_col
                                           iv_begin_row = iv_begin_row
                                           iv_end_col   = iv_end_col
                                           iv_end_row   = iv_end_row
                                           iv_date_format = iv_date_format "DC_SITTISAK++
                                 IMPORTING et_data      = et_data ).
            CATCH zcx_ca_message.
          ENDTRY.
        WHEN cc_type-ext_txt.
          TRY.
              upload_text_file( EXPORTING iv_filename  = iv_filename
                                          iv_begin_row = iv_begin_row
                                IMPORTING et_data      = et_data ).
            CATCH zcx_ca_message.
          ENDTRY.
        WHEN OTHERS.
          MESSAGE e101 WITH lv_ext RAISING ex_extension_file_not_support.
      ENDCASE.
    ELSE.
      MESSAGE e101 WITH lv_ext RAISING ex_extension_file_not_support.
    ENDIF.

  ENDMETHOD.


  METHOD is_logo_valid.
*<--- Start of Insertion By DC_SITTISAK May 17, 2018 11:37:32 AM
    SELECT SINGLE @abap_true
      FROM stxbitmaps
      INTO @rv_result
     WHERE tdname = @iv_logoname.
*---> End of Insertion By DC_SITTISAK May 17, 2018 11:37:32 AM
  ENDMETHOD.


  METHOD log_add_message.

    DATA: lw_msg TYPE bal_s_msg.

    CHECK NOT iv_handle IS INITIAL.

* build up message
    lw_msg-msgty     = sy-msgty.
    lw_msg-msgid     = sy-msgid.
    lw_msg-msgno     = sy-msgno.
    lw_msg-msgv1     = sy-msgv1.
    lw_msg-msgv2     = sy-msgv2.
    lw_msg-msgv3     = sy-msgv3.
    lw_msg-msgv4     = sy-msgv4.
    lw_msg-probclass = iv_problemclass.

    lw_msg-context-value   = iv_context.
    lw_msg-context-tabname = iv_tabname.
* add the message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg          = lw_msg
        i_log_handle     = iv_handle
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
* replace last message of log
    IF sy-subrc = 3.
*   build message for 'log is full'
      lw_msg-msgty     = 'E'.
      lw_msg-msgid     = 'SPPF'.
      lw_msg-msgno     = '801'.
      lw_msg-msgv1     = space.
      lw_msg-msgv2     = space.
      lw_msg-msgv3     = space.
      lw_msg-msgv4     = space.
      lw_msg-probclass = '1'.
      CALL FUNCTION 'BAL_LOG_MSG_REPLACE'
        EXPORTING
          i_log_handle = iv_handle
          i_s_msg      = lw_msg
        EXCEPTIONS
          OTHERS       = 0.
      rv_log_is_full = sppf_true.
    ENDIF.
  ENDMETHOD.


  METHOD log_add_message_free_text.
    "DATA lv_msgty TYPE symsgty VALUE 'S'.

    CHECK NOT iv_handle IS INITIAL.
* add the message
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = iv_handle
        i_msgty          = iv_msgty
        i_text           = iv_text
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD log_clear_all.
    DATA lt_log_handle         TYPE bal_t_logh.

    CALL FUNCTION 'BAL_GLB_SEARCH_LOG'
      IMPORTING
        e_t_log_handle = lt_log_handle
      EXCEPTIONS
        log_not_found  = 1.

    CHECK sy-subrc = 0.
    LOOP AT lt_log_handle INTO DATA(lw_log_handle).
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle  = lw_log_handle
        EXCEPTIONS
          log_not_found = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_create.
    DATA: lw_log TYPE bal_s_log.

* register handler
    DATA: lo_ta_manager      TYPE REF TO cl_os_transaction_manager,
          lo_top_transaction TYPE REF TO if_os_transaction.

    IF sv_handler_registered IS INITIAL.
*   register event handler for 'end of transaction' in order
*   to call refresh protocols
      lo_ta_manager ?= cl_os_system=>get_transaction_manager( ).

      lo_top_transaction = lo_ta_manager->if_os_transaction_manager~get_top_transaction( ).

      IF lo_top_transaction IS INITIAL.
*     no top transaction - no protokoll
        EXIT.
      ENDIF.
      SET HANDLER log_refresh FOR lo_top_transaction.
*   remember registration
      sv_handler_registered = abap_true.
    ENDIF.

* set header info
    lw_log-object     = iv_object.
    lw_log-subobject  = iv_subobject.
    lw_log-aldate     = sy-datum .
    lw_log-altime     = sy-uzeit.
    lw_log-aluser     = sy-uname.
    lw_log-extnumber  = iv_ext_no.

* create application log (processing log)
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lw_log
      IMPORTING
        e_log_handle = rv_handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD log_delete_message.
    CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
      EXPORTING
        i_log_handle  = iv_handle
      EXCEPTIONS
        log_not_found = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD log_load.
    DATA: lt_loghandles     TYPE bal_t_logh,
          lt_log_handle     TYPE bal_t_logh,
          lt_messagehandles TYPE bal_t_msgh,
          lw_log_handle     TYPE bal_s_logh,
          lw_log_filter     TYPE bal_s_lfil.

    CHECK NOT iv_handle IS INITIAL.

    APPEND iv_handle TO lt_log_handle.

* load application log
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_handle                = lt_log_handle
        i_exception_if_already_loaded = sppf_true
      IMPORTING
        e_t_log_handle                = lt_loghandles
        e_t_msg_handle                = lt_messagehandles
      EXCEPTIONS
        no_logs_specified             = 1
        log_not_found                 = 2
        log_already_loaded            = 3
        OTHERS                        = 4.
    IF sy-subrc = 3 OR sy-subrc = 2.
* protocol already exists in memory, find it
      lw_log_handle-sign   = sppf_sign_include.
      lw_log_handle-option = sppf_option_equal.
      lw_log_handle-low    = iv_handle.
      APPEND lw_log_handle TO lw_log_filter-log_handle.

      CALL FUNCTION 'BAL_GLB_SEARCH_LOG'
        EXPORTING
          i_s_log_filter = lw_log_filter
        IMPORTING
          e_t_log_handle = lt_loghandles
        EXCEPTIONS
          log_not_found  = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

* we get only one handle
    READ TABLE lt_loghandles INTO rv_loghandle INDEX 1.
  ENDMETHOD.


  METHOD log_refresh.
    DATA: lw_log_handle TYPE balloghndl.

*   remember registration done
    sv_handler_registered = space.

    LOOP AT st_log_handle_tab INTO lw_log_handle.
*   clear protocol (in memory)
      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle  = lw_log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.

*   forget logs
    CLEAR st_log_handle_tab.
  ENDMETHOD.


  METHOD log_save.

    DATA: lt_handles      TYPE bal_t_logh,
          lw_scnt         TYPE bal_s_scnt,
          lt_protocols_db TYPE bal_t_lgnm,
          lw_protocols_db TYPE bal_s_lgnm.

    CHECK NOT iv_loghandle IS INITIAL.

* are there any messages at all?
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = iv_loghandle
      IMPORTING
        e_statistics = lw_scnt
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
* error? what error?
      EXIT.
    ENDIF.

* check number of messages
    CHECK lw_scnt-msg_cnt_al GT 0.

    APPEND iv_loghandle TO lt_handles.

* check mode - online, update task?
    IF iv_update_task = 'X'.
*   in update task, commit work will follow
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_in_update_task = 'X'
          i_save_all       = ' '
          i_t_log_handle   = lt_handles
        IMPORTING
          e_new_lognumbers = lt_protocols_db
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
*     remember handles for refresh on commit work
        APPEND LINES OF lt_handles TO st_log_handle_tab.
      ENDIF.
*   there is only one protocol
      READ TABLE lt_protocols_db INDEX 1
        INTO lw_protocols_db.

*   set return value
      rv_logno = lw_protocols_db-lognumber.
    ELSE.
*   save immediately, no more commit work will follow
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_in_update_task = ' '
          i_save_all       = ' '
          i_t_log_handle   = lt_handles
        IMPORTING
          e_new_lognumbers = lt_protocols_db
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*   there is only one protocol
      READ TABLE lt_protocols_db INDEX 1
        INTO lw_protocols_db.

*   set return value
      rv_logno = lw_protocols_db-lognumber.
    ENDIF.
  ENDMETHOD.


  METHOD log_show.
    DATA: lw_profile TYPE bal_s_prof,
          lt_handles TYPE bal_t_logh.

    INSERT iv_protocol INTO TABLE lt_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = lw_profile.

* Modify the field catalog with the extra fields
    lw_profile-mess_fcat[] = it_fldcat[].
    lw_profile-use_grid    = sppf_true.
    lw_profile-start_col   = 20.
    lw_profile-start_row   = 5.
    lw_profile-end_col     = 120.
    lw_profile-pop_adjst   = sppf_true.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = lw_profile
        i_t_log_handle      = lt_handles
      EXCEPTIONS
        OTHERS              = 0.
  ENDMETHOD.


  METHOD mark_x.

    DATA(lt_components) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( iw_source ) )->components.
    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<lf_comp>).
      ASSIGN COMPONENT <lf_comp>-name OF STRUCTURE iw_source TO FIELD-SYMBOL(<lfs_source>).
      IF <lfs_source> IS NOT INITIAL.
        ASSIGN COMPONENT <lf_comp>-name OF STRUCTURE cw_target TO FIELD-SYMBOL(<lfs_target>).
        IF sy-subrc EQ 0.
          IF sy-tabix LT iv_startpos.
            <lfs_target> = <lfs_source>.
          ELSE.
            <lfs_target> = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD move_data.
    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          ls_comp   TYPE abap_compdescr.
    FIELD-SYMBOLS: <lf_comp_in>  TYPE any,
                   <lf_compx_ch> TYPE any,
                   <lf_comp_out> TYPE any.

    lo_struct ?= cl_abap_typedescr=>describe_by_data( iv_data_in ).
    LOOP AT lo_struct->components INTO ls_comp.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE iv_data_in TO  <lf_comp_in>.

      ASSIGN COMPONENT ls_comp-name OF STRUCTURE ev_data_out TO <lf_comp_out>.
      IF sy-subrc EQ 0.
        assign_value( EXPORTING iv_value = <lf_comp_in>
                      IMPORTING ev_value = <lf_comp_out>
                      EXCEPTIONS ex_conversion_error = 1
                                 OTHERS              = 2 ).
        IF sy-subrc <> 0.
          "Error from whatever reason
          IF iv_exception = abap_true.
            IF <lf_comp_in> NE space.  "Input has value
              "Conversion of & error: From & to &
              RAISE RESUMABLE EXCEPTION TYPE zcx_ca_message
                MESSAGE ID 'ZCA00'
                TYPE 'E'
                NUMBER '103'
                WITH ls_comp-name <lf_comp_in>.
            ENDIF.
          ENDIF.
* Begin of Chermaine+
        ELSE.
          IF <lf_comp_in> NE space.  "Input has value
            ASSIGN COMPONENT ls_comp-name OF STRUCTURE ch_datax_in TO <lf_compx_ch>.
            IF sy-subrc EQ 0.
*             check for type and length of field in x- structure
              DESCRIBE FIELD <lf_compx_ch> TYPE DATA(lv_type).
              DESCRIBE FIELD <lf_compx_ch> LENGTH DATA(lv_length) IN CHARACTER MODE.
              IF lv_type EQ 'C' AND lv_length EQ 1.
                <lf_compx_ch> = 'X'.
              ELSE.
                <lf_compx_ch> = <lf_comp_out>.
              ENDIF.
            ENDIF.
          ENDIF.
* End of Chermaine+
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD prepare_selscr_value.
    DATA: lt_selection_table     TYPE rsparams_tt,
          lt_selection_table_255 TYPE pivb_rsparamsl_255_t.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = iv_repid                   " Program for which selections are to be displayed
      TABLES
        selection_table     = lt_selection_table         " Table with ranges structure that contains selections
        selection_table_255 = lt_selection_table_255    " Table with ranges structure which contains selections (RSPAR
      EXCEPTIONS
        not_found           = 1
        no_report           = 2.

    DATA(lr_selpar) = VALUE hrtnm_tab_rng_fieldname( FOR lw_selpar IN it_selpar LET s = 'I' o = 'EQ' IN ( sign = s option = o low = lw_selpar ) ).

    rt_selval = do_prepare_selscr_val(
                  it_selection_table = lt_selection_table_255
                  it_selpar          = lr_selpar
                  iv_repid           = iv_repid ).
  ENDMETHOD.


  METHOD read_address.
    DATA: lt_param       TYPE abap_parmbind_tab,
          lw_param       TYPE abap_parmbind,
          lt_adr2        TYPE tty_adr2,    "Telephone
          lt_adr3        TYPE tty_adr3,    "Fax
          lt_adr6        TYPE tty_adr6,    "Email
          lw_adrc        TYPE adrc,
          lv_method_name TYPE string,
          lv_nation      TYPE ad_nation,
          lv_date        TYPE sy-datum,
          lv_time        TYPE sy-uzeit,
          lv_timezone    TYPE sy-zonlo,
          lv_timestamp   TYPE tstmp.

    CHECK iv_adrnr IS NOT INITIAL AND iv_langu IS NOT INITIAL.
* Data selection ------------------------------------------------------
    IF iv_nation IS SUPPLIED.
      lv_nation = iv_nation.
    ELSE.
      SELECT b~nation
        UP TO 1 ROWS
        INTO lv_nation
        FROM t002t AS a INNER JOIN tsadvt AS b
          ON a~spras EQ b~langu
         AND a~sptxt EQ b~nation_tex
       WHERE a~spras EQ sy-langu
         AND a~sprsl EQ iv_langu.
      ENDSELECT.
      IF sy-subrc NE 0.
        lv_nation = 'I'.
      ENDIF.
    ENDIF.

* Get Reference Date / Time
    IF iv_date IS INITIAL.
      lv_date     = sy-datum.
      lv_time     = sy-uzeit.
      lv_timezone = sy-zonlo.
    ELSE.
      lv_date     = iv_date.
      lv_time     = iv_time.
      lv_timezone = iv_timezone.
    ENDIF.

    CONVERT DATE lv_date
            TIME lv_time
       INTO TIME STAMP lv_timestamp
            TIME ZONE  lv_timezone.

* Get Address
    SELECT SINGLE * INTO lw_adrc
      FROM adrc
     WHERE addrnumber EQ iv_adrnr
       AND date_from  LE lv_date
       AND nation     EQ lv_nation
       AND date_to    GE lv_date.
    IF sy-subrc NE 0.
      IF iv_next_sel EQ abap_true.
        SELECT SINGLE * INTO lw_adrc
          FROM adrc
         WHERE addrnumber EQ iv_adrnr
           AND date_from  LE lv_date
           AND date_to    GE lv_date.
      ELSE.
        RAISE not_found.
      ENDIF.
    ENDIF.

    IF ev_tel IS SUPPLIED.
      "Telephone Numbers (Business Address Services)
      SELECT * FROM adr2
        INTO TABLE lt_adr2
       WHERE addrnumber EQ iv_adrnr
         AND flg_nouse  EQ abap_false.
    ENDIF.

    IF ev_fax IS SUPPLIED.
      "Fax Numbers (Business Address Services)
      SELECT * FROM adr3
        INTO TABLE lt_adr3
       WHERE addrnumber EQ iv_adrnr
         AND flg_nouse  EQ abap_false.
    ENDIF.

    IF ev_email IS SUPPLIED.
      "E-Mail Addresses (Business Address Services)
      SELECT * FROM adr6
        INTO TABLE lt_adr6
       WHERE addrnumber EQ iv_adrnr
         AND flg_nouse  EQ abap_false.
    ENDIF.

* Address formatting --------------------------------------------------
    lv_method_name = 'SET_FORMAT_ADDRESS'.
    add_param:      iv_langu                cc_type-export 'IV_LANGU',
                    iv_land1                cc_type-export 'IV_LAND1',
                    iv_one_number           cc_type-export 'IV_ONE_NUMBER',
                    iv_notelfax_heading     cc_type-export 'IV_NOTELFAX_HEADING',
                    iv_country_always_disp  cc_type-export 'IV_COUNTRY_ALWAYS_DISP',
                    lw_adrc                 cc_type-export 'IW_ADRC',
                    lt_adr2                 cc_type-export 'IT_ADR2',
                    lt_adr3                 cc_type-export 'IT_ADR3',
                    lt_adr6                 cc_type-export 'IT_ADR6',
                    lv_timestamp            cc_type-export 'IV_TIMESTAMP',
                    iv_name_width           cc_type-export 'IV_NAME_WIDTH',
                    iv_addr_width           cc_type-export 'IV_ADDR_WIDTH',
*<--- Start of Insertion By DC_SITTISAK Jun 6, 2018 4:57:08 PM
                    iv_separate_city1       cc_type-export 'IV_SEPARATE_CITY1'.
*---> End of Insertion By DC_SITTISAK Jun 6, 2018 4:57:08 PM

    IF iv_devtype    IS SUPPLIED OR
       iv_sapfont    IS SUPPLIED OR
       iv_fontsize   IS SUPPLIED OR
       iv_width      IS SUPPLIED OR
       iv_width_unit IS SUPPLIED.
      add_param: iv_devtype              cc_type-export 'IV_DEVTYPE',
                 iv_sapfont              cc_type-export 'IV_SAPFONT',
                 iv_fontsize             cc_type-export 'IV_FONTSIZE',
                 iv_bold                 cc_type-export 'IV_BOLD',
                 iv_italic               cc_type-export 'IV_ITALIC',
                 iv_width                cc_type-export 'IV_WIDTH',
                 iv_width_unit           cc_type-export 'IV_WIDTH_UNIT'.
    ENDIF.

    add_param_opt:  ev_name_l1              cc_type-import 'EV_NAME_L1',
                    ev_name_l2              cc_type-import 'EV_NAME_L2',
                    ev_name_l3              cc_type-import 'EV_NAME_L3',
                    ev_name_l4              cc_type-import 'EV_NAME_L4',
                    ev_addr_l1              cc_type-import 'EV_ADDR_L1',
                    ev_addr_l2              cc_type-import 'EV_ADDR_L2',
                    ev_addr_l3              cc_type-import 'EV_ADDR_L3',
                    ev_addr_l4              cc_type-import 'EV_ADDR_L4'.
    IF ev_postcode IS SUPPLIED.
      add_param_opt ev_postcode             cc_type-import 'EV_POSTCODE'.
    ENDIF.
    add_param_opt:  ev_tel                  cc_type-import 'EV_TEL',
                    ev_fax                  cc_type-import 'EV_FAX',
                    ev_email                cc_type-import 'EV_EMAIL'.
    CALL METHOD (lv_method_name)
      PARAMETER-TABLE lt_param.
  ENDMETHOD.


  METHOD read_dict_text.
    SPLIT st_info[ name = iv_selname ]-dbfield AT '-' INTO DATA(lv_tab) DATA(lv_fld).
    IF lv_tab IS NOT INITIAL AND lv_tab IS NOT INITIAL.
      rv_text = get_field_info( iv_tabname   = CONV ddobjname( lv_tab )
                                iv_fieldname = CONV fieldname( lv_fld ) )-fieldtext.
    ENDIF.
  ENDMETHOD.


  METHOD read_long_text.
    DATA: lt_ctab   TYPE tdtab_c132,
          lt_split  TYPE tline_t,
          lt_lines  TYPE tline_t,
          lt_eline  TYPE tline_t,
          lv_tabix  TYPE sy-tabix,
          lv_id     TYPE thead-tdid,
          lv_lang   TYPE thead-tdspras,
          lv_name   TYPE thead-tdname,
          lv_object TYPE thead-tdobject.

    lv_object = iv_object.
    lv_id     = iv_id.
    lv_lang   = iv_lang.
    lv_name   = iv_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id           " Text ID of text to be read
        language                = lv_lang         " Language of text to be read
        name                    = lv_name         " Name of text to be read
        object                  = lv_object       " Object of text to be read
      TABLES
        lines                   = lt_lines        " Lines of text read.
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE ex_id.
        WHEN 2.
          RAISE ex_language.
        WHEN 3.
          RAISE ex_name.
        WHEN 4.
          RAISE ex_not_found.
        WHEN 5.
          RAISE ex_object.
        WHEN 6.
          RAISE ex_reference_check.
        WHEN 7.
          RAISE ex_wrong_access_to_archive.
      ENDCASE.
    ELSE.
      LOOP AT lt_lines INTO DATA(lw_line).
        IF lines( lt_split ) EQ 0.
          DATA(lv_tdformat) = lw_line-tdformat.
        ENDIF.

        IF ( lw_line-tdformat = '*' OR lw_line-tdformat = '/' )
         AND lt_split IS NOT INITIAL.

          CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
            EXPORTING
              language          = lv_lang
              tabletype         = 'ASC'
              formatwidth       = 132
              tab_substitute    = abap_false
              lf_substitute     = abap_false
              replace_symbols   = abap_true
              replace_sapchars  = abap_true
            IMPORTING
              c_datatab         = lt_ctab
            TABLES
              itf_lines         = lt_split
            EXCEPTIONS
              invalid_tabletype = 1
              OTHERS            = 2.

          CHECK sy-subrc = 0.
          LOOP AT lt_ctab ASSIGNING FIELD-SYMBOL(<lf_tab>).
            lv_tabix = sy-tabix.

            APPEND INITIAL LINE TO lt_eline ASSIGNING FIELD-SYMBOL(<lf_line>).
            <lf_line>-tdformat = COND #( WHEN lv_tabix = 1 THEN lv_tdformat ).
            <lf_line>-tdline   = <lf_tab>.
          ENDLOOP.
* Keep line when have format
          IF sy-subrc NE 0.
            READ TABLE lt_split ASSIGNING FIELD-SYMBOL(<lf_split>) INDEX 1.
            IF sy-subrc EQ 0.
              IF <lf_split>-tdformat IS NOT INITIAL.
                LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<lf_split_add>).
                  lv_tabix = sy-tabix.

                  APPEND INITIAL LINE TO lt_eline ASSIGNING FIELD-SYMBOL(<lf_line_add>).
                  <lf_line_add>-tdformat = COND #( WHEN lv_tabix = 1 THEN lv_tdformat ).
                  <lf_line_add>-tdline   = <lf_split_add>-tdline.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.

          REFRESH: lt_split, lt_ctab.
          lv_tdformat = lw_line-tdformat.
        ENDIF.

        APPEND lw_line TO lt_split.
      ENDLOOP.

      IF lt_split IS NOT INITIAL.
        CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
          EXPORTING
            language          = lv_lang
            tabletype         = 'ASC'
            formatwidth       = 132
            tab_substitute    = abap_false
            lf_substitute     = abap_false
            replace_symbols   = abap_true
            replace_sapchars  = abap_true
          IMPORTING
            c_datatab         = lt_ctab
          TABLES
            itf_lines         = lt_split
          EXCEPTIONS
            invalid_tabletype = 1
            OTHERS            = 2.

        CHECK sy-subrc = 0.
        LOOP AT lt_ctab ASSIGNING <lf_tab>.
          lv_tabix = sy-tabix.

          APPEND INITIAL LINE TO lt_eline ASSIGNING <lf_line>.
          <lf_line>-tdformat = COND #( WHEN lv_tabix = 1 THEN lv_tdformat ).
          <lf_line>-tdline   = <lf_tab>.
        ENDLOOP.
      ENDIF.

      LOOP AT lt_eline ASSIGNING <lf_line>.
        REPLACE ALL OCCURRENCES OF '<' IN <lf_line>-tdline WITH '<(><<)>'.
      ENDLOOP.

      IF et_lines IS SUPPLIED.
        et_lines = lt_eline.
      ENDIF.

      IF ev_line IS SUPPLIED.
        LOOP AT lt_eline INTO lw_line.
          ev_line = COND string( WHEN sy-tabix = 1 THEN |{ lw_line-tdline }|
                                 ELSE |{ ev_line } { lw_line-tdline }| ).
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD read_one_time_address.
    DATA: lt_param       TYPE abap_parmbind_tab,
          lw_param       TYPE abap_parmbind,
          lw_adrc        TYPE adrc,
          lw_bsec        TYPE bsec,
          lv_method_name TYPE string,
          lv_nation      TYPE ad_nation,
          lv_date        TYPE sy-datum,
          lv_time        TYPE sy-uzeit,
          lv_timezone    TYPE sy-zonlo,
          lv_timestamp   TYPE tstmp.

    CHECK iv_bukrs IS NOT INITIAL
      AND iv_gjahr IS NOT INITIAL
      AND iv_belnr IS NOT INITIAL
      AND iv_langu IS NOT INITIAL.
* Data selection ------------------------------------------------------
    IF iv_nation IS SUPPLIED.
      lv_nation = iv_nation.
    ELSE.
      SELECT b~nation
        UP TO 1 ROWS
        INTO lv_nation
        FROM t002t AS a INNER JOIN tsadvt AS b
          ON a~spras EQ b~langu
         AND a~sptxt EQ b~nation_tex
       WHERE a~spras EQ sy-langu
         AND a~sprsl EQ iv_langu.
      ENDSELECT.
      IF sy-subrc NE 0.
        lv_nation = 'I'.
      ENDIF.
    ENDIF.

* Get Reference Date / Time
    IF iv_date IS INITIAL.
      lv_date     = sy-datum.
      lv_time     = sy-uzeit.
      lv_timezone = sy-zonlo.
    ELSE.
      lv_date     = iv_date.
      lv_time     = iv_time.
      lv_timezone = iv_timezone.
    ENDIF.

    CONVERT DATE lv_date
            TIME lv_time
       INTO TIME STAMP lv_timestamp
            TIME ZONE  lv_timezone.

* Get Address
    SELECT SINGLE * INTO lw_bsec
    FROM bsec
    WHERE bukrs = iv_bukrs
      AND belnr = iv_belnr
      AND gjahr = iv_gjahr.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.


* Address formatting --------------------------------------------------
    lv_method_name = 'SET_FORMAT_ONETIME_ADDRESS'.
    add_param:      iv_langu                cc_type-export 'IV_LANGU',
                    iv_land1                cc_type-export 'IV_LAND1',
                    iv_one_number           cc_type-export 'IV_ONE_NUMBER',
                    iv_notelfax_heading     cc_type-export 'IV_NOTELFAX_HEADING',
                    iv_country_always_disp  cc_type-export 'IV_COUNTRY_ALWAYS_DISP',
                    lw_bsec                 cc_type-export 'IW_BSEC',
                    lv_timestamp            cc_type-export 'IV_TIMESTAMP',
                    iv_name_width           cc_type-export 'IV_NAME_WIDTH',
                    iv_addr_width           cc_type-export 'IV_ADDR_WIDTH'.

    IF iv_devtype    IS SUPPLIED OR
       iv_sapfont    IS SUPPLIED OR
       iv_fontsize   IS SUPPLIED OR
       iv_width      IS SUPPLIED OR
       iv_width_unit IS SUPPLIED.
      add_param: iv_devtype              cc_type-export 'IV_DEVTYPE',
                 iv_sapfont              cc_type-export 'IV_SAPFONT',
                 iv_fontsize             cc_type-export 'IV_FONTSIZE',
                 iv_bold                 cc_type-export 'IV_BOLD',
                 iv_italic               cc_type-export 'IV_ITALIC',
                 iv_width                cc_type-export 'IV_WIDTH',
                 iv_width_unit           cc_type-export 'IV_WIDTH_UNIT'.
    ENDIF.

    add_param_opt:  ev_name_l1              cc_type-import 'EV_NAME_L1',
                    ev_name_l2              cc_type-import 'EV_NAME_L2',
                    ev_name_l3              cc_type-import 'EV_NAME_L3',
                    ev_name_l4              cc_type-import 'EV_NAME_L4',
                    ev_addr_l1              cc_type-import 'EV_ADDR_L1',
                    ev_addr_l2              cc_type-import 'EV_ADDR_L2',
                    ev_addr_l3              cc_type-import 'EV_ADDR_L3',
                    ev_addr_l4              cc_type-import 'EV_ADDR_L4',
*<--- Start of Insertion By DC_SITTISAK Aug 2, 2018 7:23:13 PM
                    ev_tax                  cc_type-import 'EV_TAX'.
*---> End of Insertion By DC_SITTISAK Aug 2, 2018 7:23:13 PM
    CALL METHOD (lv_method_name)
      PARAMETER-TABLE lt_param.
  ENDMETHOD.


  METHOD read_variable_value.
    CHECK iv_zzprog IS NOT INITIAL AND iv_zzname IS NOT INITIAL.
    IF iv_append IS INITIAL AND et_s_value IS SUPPLIED.
      REFRESH et_s_value.
    ENDIF.

    "Ranges
    IF et_s_value IS SUPPLIED.
      SELECT zzsign, zzopti, zzlow, zzhigh
*   APPENDING TABLE et_s_value
        INTO TABLE  @DATA(lt_varv)
        FROM ztca_varv
       WHERE zzprog EQ @iv_zzprog
         AND zzname EQ @iv_zzname
         AND zzacti NE @space
         AND zztype EQ @cc_type-ranges.
      IF sy-subrc NE 0.
*        RAISE EX_READ_PARAMETER_NOT_FOUND.
        MESSAGE e102(zca00) WITH iv_zzname iv_zzprog
          RAISING ex_read_parameter_not_found.
      ELSE.
        LOOP AT lt_varv ASSIGNING FIELD-SYMBOL(<lf_varv>).
          APPEND INITIAL LINE TO et_s_value ASSIGNING FIELD-SYMBOL(<lf_value>).
          ASSIGN COMPONENT 1 OF STRUCTURE <lf_value> TO FIELD-SYMBOL(<lf_sign>).
          IF sy-subrc EQ 0.
            <lf_sign> = <lf_varv>-zzsign.
          ENDIF.
          ASSIGN COMPONENT 2 OF STRUCTURE <lf_value> TO FIELD-SYMBOL(<lf_option>).
          IF sy-subrc EQ 0.
            <lf_option> = <lf_varv>-zzopti.
          ENDIF.
          ASSIGN COMPONENT 3 OF STRUCTURE <lf_value> TO FIELD-SYMBOL(<lf_low>).
          IF sy-subrc EQ 0.
            <lf_low> = <lf_varv>-zzlow.
          ENDIF.
          ASSIGN COMPONENT 4 OF STRUCTURE <lf_value> TO FIELD-SYMBOL(<lf_high>).
          IF sy-subrc EQ 0.
            <lf_high> = <lf_varv>-zzhigh.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "Single value
    IF ev_p_value IS SUPPLIED.
      SELECT zzlow
       UP TO 1 ROWS
        INTO @DATA(lv_low)
        FROM ztca_varv
       WHERE zzprog EQ @iv_zzprog
         AND zzname EQ @iv_zzname
         AND zzacti NE @space
         AND zztype EQ @cc_type-constant.
      ENDSELECT.
      IF sy-subrc NE 0.
*          RAISE EX_READ_PARAMETER_NOT_FOUND.
        MESSAGE e102(zca00) WITH iv_zzname iv_zzprog
          RAISING ex_read_parameter_not_found.
      ELSE.
        ev_p_value = lv_low.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD round_up_value.
    DATA: lv_value TYPE netwr.

    lv_value = iv_value.
    lv_value = round( val = lv_value dec = iv_decimal mode = iv_mode ).
    ev_value = lv_value.

  ENDMETHOD.


  METHOD selval.
    TRY.
        DATA(lv_type) = st_info[ name = iw_selection-selname ]-type.

        rv_text = SWITCH #( iw_selection-option WHEN 'EQ' OR '=' OR space OR 'CP' THEN |{ format_out( iv_value = iw_selection-low
                                                                                                      iv_type  = lv_type ) }|
                                                WHEN 'NE' OR '<>' OR 'NP' THEN |<>{ format_out( iv_value = iw_selection-low
                                                                                                iv_type  = lv_type ) }| ).
        rv_text = SWITCH #( iw_selection-option WHEN 'GT' OR  '>' THEN |>{ format_out( iv_value = iw_selection-low
                                                                                       iv_type  = lv_type ) }|
                                                WHEN 'GE' OR  '>=' THEN |>={ format_out( iv_value = iw_selection-low
                                                                                         iv_type  = lv_type ) }|
                                                WHEN 'LT' OR  '<' THEN |<{ format_out( iv_value = iw_selection-low
                                                                                       iv_type  = lv_type ) }|
                                                ELSE rv_text ).

        rv_text = SWITCH #( iw_selection-option WHEN 'LE' OR  '<=' THEN |<={ format_out( iv_value = iw_selection-low
                                                                                         iv_type  = lv_type ) }|
                                                WHEN 'BT' OR 'NB' THEN |{ format_out( iv_value = iw_selection-low
                                                                                      iv_type  = lv_type ) } - { format_out( iv_value = iw_selection-high
                                                                                                                             iv_type  = lv_type ) }|
                                                ELSE rv_text ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD set_format_address.
    "Note....
    "IM_DEVTYPE / IM_SAPFONT / IM_FONTSIZE / IM_BOLD / IM_ITALIC
    "These parameters will be used for Address section (not name, telephone, fax)
    TYPES:
      BEGIN OF lty_text,
        text TYPE char512,
      END   OF lty_text.

    DATA: lt_text      TYPE TABLE OF lty_text,
          lt_adr2      TYPE tty_adr2,    "Telephone
          lt_adr3      TYPE tty_adr3,    "Fax
          lt_adr6      TYPE tty_adr6,    "Email
          lw_adrc      TYPE adrc,
          lw_t005t     TYPE t005t,
          lv_city1     TYPE char200,
          lv_timestamp TYPE tstmp,
          lv_num(1)    TYPE n,
          lv_name      TYPE char512,
          lv_addr      TYPE char512,
          lv_width     TYPE i,
          lv_count     TYPE i,
          lv_string    TYPE string,
          lv_fnam      TYPE string,
          lv_length    TYPE dec11_4.

    FIELD-SYMBOLS <fs_field> TYPE any.

    CHECK iw_adrc IS NOT INITIAL.
    lw_adrc      = iw_adrc.
    lt_adr2      = it_adr2.
    lt_adr3      = it_adr3.
    lt_adr6      = it_adr6.
    lv_timestamp = iv_timestamp.

    IF iv_land1               EQ space        "No LAND1 supplied, Default to show country
    OR iv_country_always_disp IS NOT INITIAL.
      SELECT SINGLE * INTO lw_t005t
        FROM t005t
       WHERE spras EQ iv_langu
         AND land1 EQ lw_adrc-country.
    ELSE.                                     "LAND1 supplied: LAND1 <> ADRC-COUNTRY
      IF iv_land1 NE lw_adrc-country.
        SELECT SINGLE * INTO lw_t005t
          FROM t005t
         WHERE spras EQ iv_langu
           AND land1 EQ lw_adrc-country.
      ENDIF.
    ENDIF.

* Name ----------------------------------------------------------------
    lv_name = COND #( WHEN lw_adrc-name2 NE space
                      THEN |{ lw_adrc-name1 }{ lw_adrc-name2 }|
                      ELSE |{ lw_adrc-name1 }| ).
    lv_name = COND #( WHEN lw_adrc-name3 NE space
                      THEN |{ lv_name }{ lw_adrc-name3 }|
                      ELSE lv_name ).
    lv_name = |{ lv_name }{ lw_adrc-name4 }|.

    IF ev_name_l4 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l3 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l2 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l1 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.

    IF lv_count = 1.
      APPEND lv_name TO lt_text.
    ELSE.
      IF iv_name_width IS INITIAL.
        lv_width = 256.
      ELSE.
        lv_width = iv_name_width.
      ENDIF.

      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = lv_name
          outputlen           = lv_width
        TABLES
          out_lines           = lt_text
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.
    ENDIF.

    DO lv_count TIMES.
      TRY.
          DATA(lv_text) = lt_text[ sy-index ]-text.

          IF ev_name_l1 IS SUPPLIED AND ev_name_l1 = space.
            ev_name_l1 = lv_text.
          ELSEIF ev_name_l2 IS SUPPLIED AND ev_name_l2 = space.
            ev_name_l2 = lv_text.
          ELSEIF ev_name_l3 IS SUPPLIED AND ev_name_l3 = space.
            ev_name_l3 = lv_text.
          ELSEIF ev_name_l4 IS SUPPLIED AND ev_name_l4 = space.
            ev_name_l4 = lv_text.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR: lv_text.
      ENDTRY.
    ENDDO.
*    IF EV_NAME_L4 IS SUPPLIED.
*      EV_NAME_L1 = LW_ADRC-NAME1.
*      EV_NAME_L2 = LW_ADRC-NAME2.
*      EV_NAME_L3 = LW_ADRC-NAME3.
*      EV_NAME_L4 = LW_ADRC-NAME4.
*    ELSEIF EV_NAME_L3 IS SUPPLIED.
*      EV_NAME_L1 = |{ LW_ADRC-NAME1 } { LW_ADRC-NAME2 }|.
*      EV_NAME_L2 = LW_ADRC-NAME3.
*      EV_NAME_L3 = LW_ADRC-NAME4.
*    ELSEIF EV_NAME_L2 IS SUPPLIED.
*      EV_NAME_L1 = |{ LW_ADRC-NAME1 } { LW_ADRC-NAME2 }|.
*      EV_NAME_L2 = |{ LW_ADRC-NAME3 } { LW_ADRC-NAME4 }|.
*    ELSE.
*      IF LW_ADRC-NAME2 NE SPACE.
*        EV_NAME_L1 = |{ LW_ADRC-NAME1 } { LW_ADRC-NAME2 }|.
*      ELSE.
*        EV_NAME_L1 = LW_ADRC-NAME1.
*      ENDIF.
*      IF LW_ADRC-NAME3 NE SPACE.
*        EV_NAME_L1 = |{ EV_NAME_L1 } { LW_ADRC-NAME3 }|.
*      ENDIF.
*      EV_NAME_L1 = |{ EV_NAME_L1 } { LW_ADRC-NAME4 }|.
*    ENDIF.
    CONDENSE: ev_name_l1, ev_name_l2, ev_name_l3, ev_name_l4.

* Address -------------------------------------------------------------
* Do not display DUMMY postal code (99999)
    IF lw_adrc-post_code1 CO '9 '.
      CLEAR lw_adrc-post_code1.
    ENDIF.

*----------------------------------------------------------------------
* Do not display country ....
    IF iv_country_always_disp = abap_false.
      CLEAR lw_t005t-landx50.
    ENDIF.

* For other language (IM_LANGU) than English,
*   it suppose to sell only domestic since it's in local language
* So, do not display country
    IF iv_langu NE cc_type-langu_en.
      CLEAR lw_t005t-landx50.
    ENDIF.
*----------------------------------------------------------------------
    IF ev_postcode IS SUPPLIED.
      ev_postcode = lw_adrc-post_code1.
      lv_city1    = lw_adrc-city1.
    ELSE.
      IF lw_t005t-landx50 NE space.
        "City / Postal Code / Country (e.g. Bangkok 10500 Thailand)
        CONCATENATE lw_adrc-city1 lw_adrc-post_code1 lw_t005t-landx50
               INTO lv_city1 SEPARATED BY space.
      ELSE.
        "City / Postal Code (e.g. Bangkok 10500)
        CONCATENATE lw_adrc-city1 lw_adrc-post_code1
               INTO lv_city1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF lw_adrc-name_co NE space.      "Add c/o name
      "Add c/o name
      CONCATENATE lv_city1 lw_adrc-name_co
             INTO lv_city1 SEPARATED BY space.
    ENDIF.
*----------------------------------------------------------------------
    IF iv_devtype    IS SUPPLIED OR
       iv_sapfont    IS SUPPLIED OR
       iv_fontsize   IS SUPPLIED OR
       iv_width      IS SUPPLIED OR
       iv_width_unit IS SUPPLIED.
      lv_num = 1.
      concat_addr_width: 'EV_ADDR_L' lw_adrc-street,
                         'EV_ADDR_L' lw_adrc-str_suppl3,
                         'EV_ADDR_L' lw_adrc-location,
                         'EV_ADDR_L' lw_adrc-str_suppl1,
                         'EV_ADDR_L' lw_adrc-str_suppl2,
                         'EV_ADDR_L' lw_adrc-city2,
                         'EV_ADDR_L' lv_city1.
    ELSE.
      CLEAR lv_count.
      REFRESH lt_text.

      concat_addr:   lv_addr lw_adrc-street,
                     lv_addr lw_adrc-str_suppl3,
                     lv_addr lw_adrc-location,
                     lv_addr lw_adrc-str_suppl1,
                     lv_addr lw_adrc-str_suppl2,
                     lv_addr lw_adrc-city2.
*<--- Start of Deletion By DC_SITTISAK Jun 6, 2018 4:58:24 PM
*                     lv_addr lv_city1.
*---> End of Deletion By DC_SITTISAK Jun 6, 2018 4:58:24 PM
*<--- Start of Insertion By DC_SITTISAK Jun 6, 2018 4:58:38 PM
      IF iv_separate_city1 = abap_false.
        concat_addr: lv_addr lv_city1.
      ENDIF.
*---> End of Insertion By DC_SITTISAK Jun 6, 2018 4:58:38 PM

      IF ev_addr_l4 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l3 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l2 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l1 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.

      IF lv_count = 1.
        APPEND lv_addr TO lt_text.
      ELSE.
        IF iv_addr_width IS INITIAL.
          lv_width = 256.
        ELSE.
          lv_width = iv_addr_width.
        ENDIF.

        CALL FUNCTION 'RKD_WORD_WRAP'
          EXPORTING
            textline            = lv_addr
            outputlen           = lv_width
          TABLES
            out_lines           = lt_text
          EXCEPTIONS
            outputlen_too_large = 1
            OTHERS              = 2.
      ENDIF.

      DO lv_count TIMES.
        TRY.
            lv_text = lt_text[ sy-index ]-text.

            IF ev_addr_l1 IS SUPPLIED AND ev_addr_l1 = space.     "Line 1
              ev_addr_l1 = lv_text.
            ELSEIF ev_addr_l2 IS SUPPLIED AND ev_addr_l2 = space. "Line 2
              ev_addr_l2 = lv_text.
            ELSEIF ev_addr_l3 IS SUPPLIED AND ev_addr_l3 = space. "Line 3
              ev_addr_l3 = lv_text.
            ELSEIF ev_addr_l4 IS SUPPLIED AND ev_addr_l4 = space. "Line 4
              ev_addr_l4 = lv_text.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            CLEAR: lv_text.
        ENDTRY.
      ENDDO.

*<--- Start of Insertion By DC_SITTISAK Jun 6, 2018 5:03:02 PM
*Separate City1 and Postal code to another line -------------------------
      DATA(lv_lines) = lines( lt_text ).
      IF iv_separate_city1 = abap_true.
        CASE lv_lines.
          WHEN 1.
            IF ev_addr_l2 IS SUPPLIED.
              ev_addr_l2 = lv_city1.
            ELSE.
              ev_addr_l1 = |{ ev_addr_l1 } { lv_city1 }|.
            ENDIF.
          WHEN 2.
            IF ev_addr_l3 IS SUPPLIED.
              ev_addr_l3 = lv_city1.
            ELSE.
              ev_addr_l2 = |{ ev_addr_l2 } { lv_city1 }|.
            ENDIF.
          WHEN 3.
            IF ev_addr_l4 IS SUPPLIED.
              ev_addr_l4 = lv_city1.
            ELSE.
              ev_addr_l3 = |{ ev_addr_l3 } { lv_city1 }|.
            ENDIF.
          WHEN OTHERS.
            ev_addr_l4 = |{ ev_addr_l4 } { lv_city1 }|.
        ENDCASE.
      ENDIF.
*---> End of Insertion By DC_SITTISAK Jun 6, 2018 5:03:02 PM
*      IF EV_ADDR_L4 IS SUPPLIED.
*        CONCAT_ADDR:   EV_ADDR_L1   LW_ADRC-STREET,       "Line 1
*                       EV_ADDR_L2   LW_ADRC-STR_SUPPL3,   "Line 2
*                       EV_ADDR_L3   LW_ADRC-LOCATION,     "Line 3
*                       EV_ADDR_L3   LW_ADRC-CITY2,
*                       EV_ADDR_L4   LW_ADRC-STR_SUPPL1,   "Line 4
*                       EV_ADDR_L4   LW_ADRC-STR_SUPPL2,
*                       EV_ADDR_L4   LV_CITY1.
*      ELSEIF EV_ADDR_L3 IS SUPPLIED.
*        CONCAT_ADDR:   EV_ADDR_L1   LW_ADRC-STREET,       "Line 1
*                       EV_ADDR_L1   LW_ADRC-STR_SUPPL3,
*                       EV_ADDR_L2   LW_ADRC-LOCATION,     "Line 2
*                       EV_ADDR_L2   LW_ADRC-CITY2,
*                       EV_ADDR_L3   LW_ADRC-STR_SUPPL1,   "Line 3
*                       EV_ADDR_L3   LW_ADRC-STR_SUPPL2,
*                       EV_ADDR_L3   LV_CITY1.
*      ELSEIF EV_ADDR_L2 IS SUPPLIED.
*        CONCAT_ADDR:   EV_ADDR_L1   LW_ADRC-STREET,       "Line 1
*                       EV_ADDR_L1   LW_ADRC-STR_SUPPL3,
*                       EV_ADDR_L1   LW_ADRC-LOCATION,
*                       EV_ADDR_L2   LW_ADRC-CITY2,        "Line 2
*                       EV_ADDR_L2   LW_ADRC-STR_SUPPL1,
*                       EV_ADDR_L2   LW_ADRC-STR_SUPPL2,
*                       EV_ADDR_L2   LV_CITY1.
*      ELSEIF EV_ADDR_L1 IS SUPPLIED.
*        CONCAT_ADDR:   EV_ADDR_L1   LW_ADRC-STREET,       "Line 1
*                       EV_ADDR_L1   LW_ADRC-STR_SUPPL3,
*                       EV_ADDR_L1   LW_ADRC-LOCATION,
*                       EV_ADDR_L1   LW_ADRC-CITY2,
*                       EV_ADDR_L1   LW_ADRC-STR_SUPPL1,
*                       EV_ADDR_L1   LW_ADRC-STR_SUPPL2,
*                       EV_ADDR_L1   LV_CITY1.
*      ENDIF.
    ENDIF.
    CONDENSE: ev_addr_l1, ev_addr_l2, ev_addr_l3, ev_addr_l4.

* Telephone and Extension (ADR2) --------------------------------------
    IF ev_tel IS SUPPLIED.
      DELETE lt_adr2 WHERE persnumber IS NOT INITIAL.

      "Get only Telephone number in validity period
      DELETE lt_adr2 WHERE ( valid_from GT lv_timestamp
                       AND   valid_from NE space )
                        OR ( valid_to   LT lv_timestamp
                       AND   valid_to   NE space ).

      "Display Default Telephone number first
      SORT lt_adr2 BY flgdefault DESCENDING
                      consnumber.

      IF iv_one_number IS NOT INITIAL.
        DELETE lt_adr2 WHERE flgdefault NE abap_true.
      ENDIF.

      LOOP AT lt_adr2 INTO DATA(ls_adr2).
        CALL METHOD get_telfax
          EXPORTING
            iv_langu            = iv_langu
            iv_type             = cc_type_tel
            iv_number           = ls_adr2-tel_number
            iv_ext              = ls_adr2-tel_extens
            iv_notelfax_heading = iv_notelfax_heading
          CHANGING
            cv_number_text      = ev_tel.
      ENDLOOP.
    ENDIF.

* Fax and Extension (ADR3) --------------------------------------------
    IF ev_fax IS SUPPLIED.
      DELETE lt_adr3 WHERE persnumber IS NOT INITIAL.

      "Get only Fax number in validity period
      DELETE lt_adr3 WHERE ( valid_from GT lv_timestamp
                       AND   valid_from NE space )
                        OR ( valid_to   LT lv_timestamp
                       AND   valid_to   NE space ).

      "Display Default Fax number first
      SORT lt_adr3 BY flgdefault DESCENDING
                      consnumber.

      IF iv_one_number IS NOT INITIAL.
        DELETE lt_adr3 WHERE flgdefault NE abap_true.
      ENDIF.

      LOOP AT lt_adr3 INTO DATA(ls_adr3).
        CALL METHOD get_telfax
          EXPORTING
            iv_langu            = iv_langu
            iv_type             = cc_type_fax
            iv_number           = ls_adr3-fax_number
            iv_ext              = ls_adr3-fax_extens
            iv_notelfax_heading = iv_notelfax_heading
          CHANGING
            cv_number_text      = ev_fax.
      ENDLOOP.
    ENDIF.

* Email (ADR6) --------------------------------------------------------
    IF ev_email IS SUPPLIED.
      DELETE lt_adr6 WHERE persnumber IS NOT INITIAL.

      "Get only E-mail address in validity period
      DELETE lt_adr6 WHERE ( valid_from GT lv_timestamp
                       AND   valid_from NE space )
                        OR ( valid_to   LT lv_timestamp
                       AND   valid_to   NE space ).

      "Display default E-mail address first
      SORT lt_adr6 BY flgdefault DESCENDING
                      consnumber.

      IF iv_one_number IS NOT INITIAL.
        DELETE lt_adr6 WHERE flgdefault NE abap_true.
      ENDIF.

      "Prepare E-mail address
      LOOP AT lt_adr6 INTO DATA(ls_adr6)
                     WHERE smtp_addr IS NOT INITIAL.
        IF ev_email IS INITIAL.
          ev_email = ls_adr6-smtp_addr.
        ELSE.
          CONCATENATE ev_email ls_adr6-smtp_addr
                 INTO ev_email SEPARATED BY '; '.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD set_format_onetime_address.
    "Note....
    "IM_DEVTYPE / IM_SAPFONT / IM_FONTSIZE / IM_BOLD / IM_ITALIC
    "These parameters will be used for Address section (not name, telephone, fax)
    TYPES:
      BEGIN OF lty_text,
        text TYPE char512,
      END   OF lty_text.

    DATA: lt_text      TYPE TABLE OF lty_text,
          lw_bsec      TYPE bsec,
          lw_t005t     TYPE t005t,
          lv_city1     TYPE char200,
          lv_timestamp TYPE tstmp,
          lv_num(1)    TYPE n,
          lv_name      TYPE char512,
          lv_addr      TYPE char512,
          lv_width     TYPE i,
          lv_count     TYPE i,
          lv_string    TYPE string,
          lv_fnam      TYPE string,
          lv_length    TYPE dec11_4.

    FIELD-SYMBOLS <fs_field> TYPE any.

    CHECK iw_bsec IS NOT INITIAL.
    lw_bsec      = iw_bsec.
    lv_timestamp = iv_timestamp.

    IF iv_land1               EQ space        "No LAND1 supplied, Default to show country
    OR iv_country_always_disp IS NOT INITIAL.
      SELECT SINGLE * INTO lw_t005t
        FROM t005t
       WHERE spras EQ iv_langu
         AND land1 EQ lw_bsec-land1.
    ELSE.                                     "LAND1 supplied: LAND1 <> ADRC-COUNTRY
      IF iv_land1 NE lw_bsec-land1.
        SELECT SINGLE * INTO lw_t005t
          FROM t005t
         WHERE spras EQ iv_langu
           AND land1 EQ lw_bsec-land1.
      ENDIF.
    ENDIF.

* Name ----------------------------------------------------------------
    lv_name = COND #( WHEN lw_bsec-name2 NE space
                      THEN |{ lw_bsec-name1 } { lw_bsec-name2 }|
                      ELSE |{ lw_bsec-name1 }| ).
    lv_name = COND #( WHEN lw_bsec-name3 NE space
                      THEN |{ lv_name } { lw_bsec-name3 }|
                      ELSE lv_name ).
    lv_name = |{ lv_name } { lw_bsec-name4 }|.

    IF ev_name_l4 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l3 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l2 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.
    IF ev_name_l1 IS SUPPLIED.
      ADD 1 TO lv_count.
    ENDIF.

    IF lv_count = 1.
      APPEND lv_name TO lt_text.
    ELSE.
      IF iv_name_width IS INITIAL.
        lv_width = 256.
      ELSE.
        lv_width = iv_name_width.
      ENDIF.

      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = lv_name
          outputlen           = lv_width
        TABLES
          out_lines           = lt_text
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.
    ENDIF.

    DO lv_count TIMES.
      TRY.
          DATA(lv_text) = lt_text[ sy-index ]-text.

          IF ev_name_l1 IS SUPPLIED AND ev_name_l1 = space.
            ev_name_l1 = lv_text.
          ELSEIF ev_name_l2 IS SUPPLIED AND ev_name_l2 = space.
            ev_name_l2 = lv_text.
          ELSEIF ev_name_l3 IS SUPPLIED AND ev_name_l3 = space.
            ev_name_l3 = lv_text.
          ELSEIF ev_name_l4 IS SUPPLIED AND ev_name_l4 = space.
            ev_name_l4 = lv_text.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR: lv_text.
      ENDTRY.
    ENDDO.
    CONDENSE: ev_name_l1, ev_name_l2, ev_name_l3, ev_name_l4.

* Address -------------------------------------------------------------
* Do not display DUMMY postal code (99999)
    IF lw_bsec-pstlz CO '9 '.
      CLEAR lw_bsec-pstlz.
    ENDIF.

*----------------------------------------------------------------------
* Do not display country ....
    IF iv_country_always_disp = abap_false.
      CLEAR lw_t005t-landx50.
    ENDIF.

* For other language (IM_LANGU) than English,
*   it suppose to sell only domestic since it's in local language
* So, do not display country
    IF iv_langu NE cc_type-langu_en.
      CLEAR lw_t005t-landx50.
    ENDIF.
*----------------------------------------------------------------------
    IF iv_devtype    IS SUPPLIED OR
       iv_sapfont    IS SUPPLIED OR
       iv_fontsize   IS SUPPLIED OR
       iv_width      IS SUPPLIED OR
       iv_width_unit IS SUPPLIED.
      lv_num = 1.
      concat_addr_width: 'EV_ADDR_L' lw_bsec-stras,
                         'EV_ADDR_L' lw_bsec-ort01,
                         'EV_ADDR_L' lw_bsec-j_1kftbus,
                         'EV_ADDR_L' lw_bsec-j_1kftind,
                         'EV_ADDR_L' lw_bsec-pstlz.
    ELSE.
      CLEAR lv_count.
      REFRESH lt_text.

      concat_addr:   lv_addr lw_bsec-stras,
                     lv_addr lw_bsec-ort01,
                     lv_addr lw_bsec-j_1kftbus,
                     lv_addr lw_bsec-j_1kftind,
                     lv_addr lw_bsec-pstlz.

      IF ev_addr_l4 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l3 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l2 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.
      IF ev_addr_l1 IS SUPPLIED.
        ADD 1 TO lv_count.
      ENDIF.

      IF lv_count = 1.
        APPEND lv_addr TO lt_text.
      ELSE.
        IF iv_addr_width IS INITIAL.
          lv_width = 256.
        ELSE.
          lv_width = iv_addr_width.
        ENDIF.

        CALL FUNCTION 'RKD_WORD_WRAP'
          EXPORTING
            textline            = lv_addr
            outputlen           = lv_width
          TABLES
            out_lines           = lt_text
          EXCEPTIONS
            outputlen_too_large = 1
            OTHERS              = 2.
      ENDIF.

      DO lv_count TIMES.
        TRY.
            lv_text = lt_text[ sy-index ]-text.

            IF ev_addr_l1 IS SUPPLIED AND ev_addr_l1 = space.     "Line 1
              ev_addr_l1 = lv_text.
            ELSEIF ev_addr_l2 IS SUPPLIED AND ev_addr_l2 = space. "Line 2
              ev_addr_l2 = lv_text.
            ELSEIF ev_addr_l3 IS SUPPLIED AND ev_addr_l3 = space. "Line 3
              ev_addr_l3 = lv_text.
            ELSEIF ev_addr_l4 IS SUPPLIED AND ev_addr_l4 = space. "Line 4
              ev_addr_l4 = lv_text.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            CLEAR: lv_text.
        ENDTRY.
      ENDDO.

    ENDIF.
    CONDENSE: ev_addr_l1, ev_addr_l2, ev_addr_l3, ev_addr_l4.

*<--- Start of Insertion By DC_SITTISAK Aug 2, 2018 7:24:15 PM
* Tax ID -------------------------------------------------------------
    ev_tax = iw_bsec-stcd3.
*---> End of Insertion By DC_SITTISAK Aug 2, 2018 7:24:15 PM
  ENDMETHOD.


  METHOD set_ranges.
    et_range = COND #( WHEN iv_append EQ abap_true THEN et_range ).

    APPEND INITIAL LINE TO et_range ASSIGNING FIELD-SYMBOL(<lf_range>).
    ASSIGN COMPONENT 1 OF STRUCTURE <lf_range> TO FIELD-SYMBOL(<lf_sign>).
    IF <lf_sign> IS ASSIGNED.
      <lf_sign> = iv_sign.
    ENDIF.
    ASSIGN COMPONENT 2 OF STRUCTURE <lf_range> TO FIELD-SYMBOL(<lf_option>).
    IF <lf_option> IS ASSIGNED.
      <lf_option> = iv_option.
    ENDIF.
    ASSIGN COMPONENT 3 OF STRUCTURE <lf_range> TO FIELD-SYMBOL(<lf_low>).
    IF <lf_low> IS ASSIGNED.
      <lf_low> = iv_low.
    ENDIF.
    ASSIGN COMPONENT 4 OF STRUCTURE <lf_range> TO FIELD-SYMBOL(<lf_high>).
    IF <lf_high> IS ASSIGNED AND iv_high IS NOT INITIAL.
      <lf_high> = iv_high.
    ENDIF.
  ENDMETHOD.


  METHOD set_value_dynamic.
    ASSIGN COMPONENT iv_field OF STRUCTURE cw_line TO FIELD-SYMBOL(<lf_field>).
    IF <lf_field> IS ASSIGNED.  "Field found in dynamic structure
      IF iv_sum = abap_true. "Add value to "number" column
        DESCRIBE FIELD iv_value TYPE DATA(lv_type).
        IF lv_type = 'P'.
          ADD iv_value TO <lf_field>.  "Add up values
        ELSE.
          IF iv_dup IS INITIAL. "Duplicate not allowed
            SPLIT <lf_field> AT iv_separ INTO TABLE DATA(lt_val).
            READ TABLE lt_val TRANSPORTING NO FIELDS
                              WITH KEY table_line = iv_value.
            IF sy-subrc EQ 0. "
              RETURN.
            ENDIF.
          ENDIF.
          IF <lf_field> IS NOT INITIAL.
            <lf_field> = |{ <lf_field> }{ iv_separ }{ iv_value }|.
          ELSE.
            <lf_field> = iv_value.
          ENDIF.
        ENDIF.
      ELSE.
        assign_value( EXPORTING iv_value = iv_value       ##SUBRC_OK
                      IMPORTING ev_value = <lf_field>
                      EXCEPTIONS ex_conversion_error = 1 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD spell_amount.
    DATA: lw_spell      TYPE spell,
          lt_tcurt_word TYPE TABLE OF ztca_tcurt_word,
          lw_zspell     TYPE zsca_spell,
          lv_plural     TYPE c,
          lv_spelldec   TYPE spell-decimal,
          lv_decimal    TYPE ztca_tcurt_word-decamt,
          lv_separated  TYPE char5.
    CONSTANTS:
          lc_and   TYPE char5 VALUE ' and '.

    IF sr_langu_e[] IS INITIAL.
      read_variable_value( EXPORTING iv_zzprog  = 'SAPLZTCA_TCURT_WORD'
                                     iv_zzname  = 'LANGUAGE_ENGLISH'
                           IMPORTING et_s_value = sr_langu_e[] ).
    ENDIF.

** CHINESE YUAN with CHINESE LANGUAGE ---------------------------------
    IF iv_language = cc_type-langu_zh.   "ZH.
      "Spell amount in Chinese Yuan with Chinese character
      " ... is handled separatedly
      CLEAR lw_zspell.
      spell_chinese( EXPORTING iv_amount = iv_amount
                     CHANGING  cw_zspell = lw_zspell ).
      ew_zspell = lw_zspell.
    ELSE.
** ALL OTHER CASES ----------------------------------------------------
      "Get denomination currency word (e.g. Dollar/Cent, Baht/Satang)
      SELECT *
        FROM ztca_tcurt_word
        INTO TABLE lt_tcurt_word
        WHERE waers EQ iv_currency
          AND langu EQ iv_language.
      IF sy-subrc <> 0.
        RAISE ex_not_found.
      ENDIF.

      "Use standard functionality to spell
      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = iv_amount
          currency  = iv_currency
          language  = iv_language
        IMPORTING
          in_words  = lw_spell
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.
      CASE sy-subrc.
        WHEN 0.
          "OK
        WHEN 1.
          RAISE ex_not_found.
        WHEN 2.
          RAISE ex_too_large.
        WHEN OTHERS.
          RAISE ex_spell_error.
      ENDCASE.

      "Spell with Denomination
      CLEAR lw_zspell.
      MOVE-CORRESPONDING lw_spell TO lw_zspell.

      IF lw_spell-number > 1.
        lv_plural = abap_true.
      ELSE.
        lv_plural = abap_false.
      ENDIF.

      IF lw_spell-currdec NE 0.
        IF lw_spell-currdec < 3.
          lv_spelldec = lw_spell-decimal+0(lw_spell-currdec).
        ELSE.
          lv_spelldec = lw_spell-decimal+0(*).
        ENDIF.
      ENDIF.
      IF lv_spelldec = 0.
        lv_decimal = 0.    "Decimal amount = 0         (= 0)
      ELSEIF lv_spelldec = 1.
        lv_decimal = 1.    "Decimal amount = 1         (= 1)
      ELSE.
        lv_decimal = 2.    "Decimal amount more than 1 (> 1)
      ENDIF.

      LOOP AT lt_tcurt_word INTO DATA(lw_tcurt_word)
        WHERE plural = lv_plural
          AND decamt = lv_decimal.
        "Found denom. currency with both plural and decimal amount match
        EXIT.
      ENDLOOP.
      IF lw_tcurt_word IS INITIAL.
        "Not found with both plural and decimal amount
        "-> Use only decimal amount (in case no plural for this currency)
        LOOP AT lt_tcurt_word INTO lw_tcurt_word
            WHERE decamt = lv_decimal.
          EXIT.
        ENDLOOP.

        IF lw_tcurt_word IS INITIAL.
          "Still not found
          SUBTRACT 1 FROM lv_decimal.
          LOOP AT lt_tcurt_word INTO lw_tcurt_word
              WHERE decamt = lv_decimal.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF lw_tcurt_word IS INITIAL.
        "Still not found -> raise error
        RAISE ex_spell_error.
      ENDIF.


** CONCATENATE WORD AND CURRENCY UNIT TOGETHER ------------------------
      "INTEGER part
      IF lw_spell-number IS NOT INITIAL.
        CONCATENATE lw_spell-word lw_tcurt_word-word
               INTO lw_zspell-word_integer SEPARATED BY space.
      ENDIF.

      "DECIMAL part (only if decimal is not 0)
      IF lw_spell-decimal IS NOT INITIAL.
        CONCATENATE lw_spell-decword lw_tcurt_word-decword
               INTO lw_zspell-word_decimal SEPARATED BY space.
      ENDIF.

      "Convert first letter to uppercase, all other with lowercase
      IF iv_language IN sr_langu_e[].
        firstchar_upper: lw_zspell-word_integer,
                         lw_zspell-word_decimal,
                         lw_zspell-word_full.
      ENDIF.

** FORMATTING ---------------------------------------------------------
      IF iv_language IN sr_langu_e[].
        "English -> use word 'and' to separate
        "           between INTEGER and DECIMAL part
        CLEAR lv_separated.
        IF  lw_zspell-word_integer IS NOT INITIAL
        AND lw_zspell-word_decimal IS NOT INITIAL.
          lv_separated = lc_and.
        ELSE.
          CLEAR lv_separated.
        ENDIF.
        CONCATENATE lw_zspell-word_integer lw_zspell-word_decimal
               INTO lw_zspell-word_full SEPARATED BY lv_separated.
        CONDENSE lw_zspell-word_full.
      ELSE.
        "Other language -> just concat INTEGER and DECIMAL part together
        CONCATENATE lw_zspell-word_integer lw_zspell-word_decimal
               INTO lw_zspell-word_full SEPARATED BY space.
      ENDIF.

      "Thai language -> no gaps between word
      IF iv_language = cc_type-langu_th. "TH.
        CONDENSE: lw_zspell-word_integer NO-GAPS,
                  lw_zspell-word_decimal NO-GAPS,
                  lw_zspell-word_full    NO-GAPS.
      ENDIF.

      "Set text format (only for English Language)
      " IM_TEXTFORMAT
      " - A  First letter in capital letter, other in lower case
      " - B  All upper case
      " - C  All lower case
      IF iv_language IN sr_langu_e[].
        CASE iv_textformat.
          WHEN cc_type-format_a   "First letter in capital letter, other in lower case
            OR space.             "Default is A
            "Do nothing, already in this format
            REPLACE FIRST OCCURRENCE OF:
              ' Us ' IN lw_zspell-word_full WITH ' US ',
              ' Sg ' IN lw_zspell-word_full WITH ' SG '.
          WHEN cc_type-format_b.  "All upper case
            TRANSLATE: lw_zspell-word_integer TO UPPER CASE,
                       lw_zspell-word_decimal TO UPPER CASE,
                       lw_zspell-word_full    TO UPPER CASE.
          WHEN cc_type-format_c.  "All lower case
            TRANSLATE: lw_zspell-word_integer TO LOWER CASE,
                       lw_zspell-word_decimal TO LOWER CASE,
                       lw_zspell-word_full    TO LOWER CASE.
          WHEN OTHERS.
            RAISE ex_incorrect_text_format.
        ENDCASE.
      ENDIF.

      ew_zspell = lw_zspell.
    ENDIF.
  ENDMETHOD.


  METHOD spell_chinese.
    DATA: lv_units(30)     VALUE '分角圆拾佰仟萬拾佰仟亿拾佰仟万',
          lv_digts(20)     VALUE '零壹贰叁肆伍陆柒捌玖',
          lv_money_str(33),
          lv_cword(2),
          lv_weight(2),
          lv_amount        TYPE fiappt_amount,
          lv_lastd         TYPE n,
          lv_curntd        TYPE n,
          lv_units_off     TYPE i,
          lv_curnt_off     TYPE i,
          lv_i             TYPE i.

    lv_amount = iv_amount.

    CLEAR cw_zspell-word_full.
    IF lv_amount = 0.
      cw_zspell-word_full = '零'.
      EXIT.
    ENDIF.

    lv_money_str = lv_amount.
    CONDENSE lv_money_str NO-GAPS.
    IF lv_money_str CN '0123456789. '.
      RAISE ex_wrong_money.
    ENDIF.

    IF lv_money_str CS '.'.
      lv_i                  = sy-fdpos + 1.
      lv_money_str+sy-fdpos = lv_money_str+lv_i.
    ENDIF.
    CONDENSE lv_money_str NO-GAPS.

* clear:ps_in_words-word_full,units_off.
    lv_lastd     = 0.
    lv_curnt_off = strlen( lv_money_str ) - 1.
    WHILE lv_curnt_off >= 0.
      lv_curntd = lv_money_str+lv_curnt_off(1).
      lv_i      = lv_curntd.
      lv_cword  = lv_digts+lv_i(1).
      lv_weight = lv_units+lv_units_off(1).
      lv_i      = lv_units_off / 1.
      IF lv_curntd = 0.             "Current digit is 0
        IF lv_i = 2 OR lv_i = 6 OR lv_i = 10.
          CLEAR: lv_cword.
          IF lv_curnt_off = 0.
            CLEAR: lv_weight.
          ENDIF.
        ELSEIF lv_lastd = 0.
          CLEAR: lv_cword,lv_weight.
        ELSE.
          CLEAR: lv_weight.
        ENDIF.
      ENDIF.
      CONCATENATE lv_cword lv_weight cw_zspell-word_full
             INTO cw_zspell-word_full.
      lv_lastd = lv_curntd.
      SUBTRACT 1 FROM lv_curnt_off.
      ADD 1 TO lv_units_off.
    ENDWHILE.
    IF cw_zspell-word_full NS '分'.
      CONCATENATE cw_zspell-word_full '整'
             INTO cw_zspell-word_full.
    ELSE.
      lv_cword = cw_zspell-word_full.
      IF lv_cword = '零'.
        SHIFT cw_zspell-word_full BY 1 PLACES.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD spell_quantity.
*"----------------------------------------------------------------------
** NOTE
*  - Decimal place of IM_QUANTITY variable should be consistent with
*    parameter IM_DECIMAL_PLACE
*  - IM_LANGUAGE is now supporting only TH and EN
*----------------------------------------------------------------------
    "** Decimal allowed for this FM ***
    CONSTANTS: lc_decimal TYPE i VALUE 6.

    DATA: lw_spell_integer TYPE spell,
          lw_spell_decimal TYPE spell,
          lv_decimal_place TYPE t006-decan,
          lv_spell_unit    TYPE t006a-msehl,
          lv_cuky          TYPE sy-waers,
          lv_shift_factor  TYPE i,
          lv_decimal       TYPE p DECIMALS lc_decimal,
          lv_decimal0      TYPE p DECIMALS 0,
          lv_digi          TYPE n LENGTH 2,
          lv_field         TYPE string,
          lv_spell_decimal TYPE string,
          lv_dot           TYPE string,
          lv_add           TYPE c.

    FIELD-SYMBOLS: <lf_word> TYPE any.

* Get decimal place of Unit
    SELECT SINGLE decan
      INTO lv_decimal_place
      FROM t006
      WHERE msehi EQ iv_unit.
    IF sy-subrc NE 0.
      RAISE ex_unit_not_found.
    ENDIF.

* Get Unit text
    SELECT SINGLE msehl
      INTO lv_spell_unit
      FROM t006a
      WHERE spras EQ iv_language
        AND msehi EQ iv_unit.
    IF sy-subrc NE 0.
      RAISE ex_unit_not_found.
    ENDIF.

* First SPELL_AMOUNT to get integer part....
    lv_cuky = iv_decimal_place. "lv_decimal_place.
    CONDENSE lv_cuky.
    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        amount    = iv_quantity
        currency  = lv_cuky
        language  = iv_language
      IMPORTING
        in_words  = lw_spell_integer
      EXCEPTIONS
        not_found = 1
        too_large = 2
        OTHERS    = 3.
    IF sy-subrc NE 0.
      RAISE ex_not_found.
    ENDIF.

* Next SPELL_AMOUNT to get decimal part...
    lv_decimal      = frac( iv_quantity ).
    lv_shift_factor = ipow( base = 10
                            exp  = lc_decimal ).
    lv_decimal0     = lv_decimal * lv_shift_factor.

    IF lv_decimal0 IS NOT INITIAL.
      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = lv_decimal0
          currency  = '0'
          language  = iv_language
        IMPORTING
          in_words  = lw_spell_decimal
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.
      IF sy-subrc NE 0.
        RAISE ex_not_found.
      ENDIF.

      CLEAR: lv_spell_decimal, lv_add.
      lv_digi = 1.
      DO lc_decimal TIMES.
        UNASSIGN <lf_word>.
        CONCATENATE 'LW_SPELL_DECIMAL-DIG' lv_digi INTO lv_field.
        ASSIGN (lv_field) TO <lf_word>.

        ADD 1 TO lv_digi.

        CHECK <lf_word> IS ASSIGNED.
        IF ( <lf_word> = 'ZERO' OR <lf_word> = 'ศูนย์' OR <lf_word> = '零' )
          AND lv_add = space.
          CONTINUE.
        ELSE.
          lv_add = abap_true.
        ENDIF.

        CONCATENATE <lf_word> lv_spell_decimal
               INTO lv_spell_decimal SEPARATED BY space.
      ENDDO.
      CONDENSE lv_spell_decimal.
    ENDIF.

    CASE iv_language.
      WHEN cc_type-langu_en.
        lv_dot = 'POINT'.
      WHEN cc_type-langu_th.
        lv_dot = 'จุด'.
      WHEN cc_type-langu_zh.
        lv_dot = '点'.
    ENDCASE.

    CLEAR ew_zspell.
    IF lv_decimal0 IS NOT INITIAL.
      ew_zspell-word_integer = lw_spell_integer-word.
      ew_zspell-word_decimal = lv_spell_decimal.
      CONCATENATE ew_zspell-word_integer lv_dot
             INTO ew_zspell-word_full SEPARATED BY space.
      CONCATENATE ew_zspell-word_full ew_zspell-word_decimal
             INTO ew_zspell-word_full SEPARATED BY space.
      CONCATENATE ew_zspell-word_full lv_spell_unit
             INTO ew_zspell-word_full  SEPARATED BY space.
    ELSE.
      ew_zspell-word_integer = lw_spell_integer-word.
      ew_zspell-word_decimal = space.
      CONCATENATE lw_spell_integer-word lv_spell_unit
             INTO ew_zspell-word_full SEPARATED BY space.
    ENDIF.

    IF iv_language = cc_type-langu_th OR
       iv_language = cc_type-langu_zh.
      CONDENSE:  ew_zspell-word_integer NO-GAPS,
                 ew_zspell-word_decimal NO-GAPS,
                 ew_zspell-word_full    NO-GAPS.
    ELSEIF iv_language = cc_type-langu_en.
      TRANSLATE: ew_zspell-word_integer TO UPPER CASE,
                 ew_zspell-word_decimal TO UPPER CASE,
                 ew_zspell-word_full    TO UPPER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD upload_excel_file.

    DATA: lt_intern TYPE TABLE OF zsca_alsmex_tabline,
          lo_line   TYPE REF TO data,
          lv_type   TYPE char1.

    FIELD-SYMBOLS:
      <lf_line>  TYPE any,
      <lf_field> TYPE any.

    CALL FUNCTION 'ZCA_ALSM_EXCEL_TO_INTERNAL_TAB'
      EXPORTING
        filename                = iv_filename
        i_begin_col             = iv_begin_col
        i_begin_row             = iv_begin_row
        i_end_col               = iv_end_col
        i_end_row               = iv_end_row
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc = 0 AND NOT lt_intern[] IS INITIAL.
      CREATE DATA lo_line LIKE LINE OF et_data.
      ASSIGN lo_line->* TO <lf_line>.

      LOOP AT lt_intern INTO DATA(lw_intern).
        ASSIGN COMPONENT lw_intern-col OF STRUCTURE <lf_line> TO <lf_field>.
        IF <lf_field> IS ASSIGNED.
          DESCRIBE FIELD <lf_field> TYPE lv_type.
          IF lv_type = cc_type-date.
            "Expected date format from excel should be MM/DD/YYYY
            CALL FUNCTION 'KCD_EXCEL_DATE_CONVERT'
              EXPORTING
                excel_date  = lw_intern-value
                date_format = iv_date_format
              IMPORTING
                sap_date    = <lf_field>.
          ELSE.
            assign_value(
              EXPORTING
                iv_value            = lw_intern-value
              IMPORTING
                ev_value            = <lf_field>
              EXCEPTIONS
                ex_conversion_error = 1
                OTHERS              = 2
            ).
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE zcx_ca_message
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.
        ENDIF.
        AT END OF row.
          APPEND <lf_line> TO et_data.
          CLEAR <lf_line>.
        ENDAT.
      ENDLOOP.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ca_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD upload_text_file.

    DATA: lt_upload TYPE ctty_upload,
          lt_text   TYPE TABLE OF text1024,
          lw_text   TYPE text1024,
          lo_line   TYPE REF TO data,
          lv_type   TYPE char1.

    FIELD-SYMBOLS:
      <lf_line>  TYPE any,
      <lf_field> TYPE any.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
      TABLES
        data_tab                = lt_upload
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc = 0 AND lt_text[] IS INITIAL.
      CREATE DATA lo_line LIKE LINE OF et_data.
      ASSIGN lo_line->* TO <lf_line>.

* Delete xxx header line
      LOOP AT lt_upload INTO DATA(lw_upload)
                        FROM iv_begin_row.
        REFRESH lt_text.
        SPLIT lw_upload-text AT cl_abap_char_utilities=>horizontal_tab
         INTO TABLE lt_text.

        UNASSIGN <lf_field>.
        IF lt_text[] IS NOT INITIAL.
          LOOP AT lt_text INTO lw_text.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE <lf_line> TO <lf_field>.
            IF <lf_field> IS ASSIGNED.
              DESCRIBE FIELD <lf_field> TYPE lv_type.
              IF lv_type = cc_type-date.
                CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
                  EXPORTING
                    input  = lw_text
                  IMPORTING
                    output = <lf_field>.
              ELSE.
                TRY.
                    <lf_field> = lw_text.
                  CATCH cx_sy_conversion_no_number.
                    RAISE EXCEPTION TYPE zcx_ca_message
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ENDTRY.
              ENDIF.
            ENDIF.
          ENDLOOP.

          APPEND <lf_line> TO et_data.
          CLEAR <lf_line>.
        ENDIF.
      ENDLOOP.
    ELSE.
      RAISE EXCEPTION TYPE zcx_ca_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
