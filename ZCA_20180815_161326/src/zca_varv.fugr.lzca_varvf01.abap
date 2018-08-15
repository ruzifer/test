*----------------------------------------------------------------------*
***INCLUDE LZCA_VARVF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form UPDATE_INFORMATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_information.

  TYPES: BEGIN OF ty_total.
      INCLUDE STRUCTURE ztca_varv.
      INCLUDE STRUCTURE vimtbflags.
  TYPES: END   OF ty_total.

* Variable TOTAL is not transparent, this structure is how we access
* the individual fields.
  DATA: lw_total TYPE ty_total.
  DATA: lv_tabix LIKE sy-tabix. "Index to note the lines found

*Add Changed-By and Changed-On for changed entries.
  LOOP AT total.
    lw_total = total.

    CHECK lw_total-vim_action = neuer
       OR lw_total-vim_action = aendern.

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
    ELSE.
      CLEAR lv_tabix.
    ENDIF.

    CASE lw_total-vim_action.
      WHEN neuer.       "N - New
        lw_total-zzcrby = sy-uname.
        lw_total-zzcron = sy-datum.
      WHEN aendern.     "U - Update
        lw_total-zzchby = sy-uname.
        lw_total-zzchon = sy-datum.
    ENDCASE.

    total = lw_total.
    MODIFY total.

    CHECK lv_tabix GT 0.
    extract = total.
    MODIFY extract INDEX lv_tabix.
  ENDLOOP.

ENDFORM.
