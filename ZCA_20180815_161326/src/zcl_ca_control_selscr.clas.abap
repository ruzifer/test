CLASS zcl_ca_control_selscr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_ca_control_selscr.
    ALIASES alv_user_command
      FOR zif_ca_control_selscr~alv_user_command.
    ALIASES alv_double_click
      FOR zif_ca_control_selscr~alv_double_click.
    ALIASES alv_link_click
      FOR zif_ca_control_selscr~alv_link_click.
    ALIASES alv_top_of_page
      FOR zif_ca_control_selscr~alv_top_of_page.
    ALIASES alv_end_of_page
      FOR zif_ca_control_selscr~alv_end_of_page.

    "TREE
    ALIASES alv_tree_double_click
      FOR zif_ca_control_selscr~alv_tree_double_click.

    ALIASES alv_tree_checkbox_chg
      FOR zif_ca_control_selscr~alv_tree_checkbox_chg.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ca_control_selscr IMPLEMENTATION.

ENDCLASS.
