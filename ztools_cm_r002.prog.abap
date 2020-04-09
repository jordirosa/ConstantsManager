*&---------------------------------------------------------------------*
*& Report ZTOOLS_CM_R002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTOOLS_CM_R002.

TABLES: sscrfields.

CONSTANTS: c_const_group TYPE ztools_const_group VALUE 'ZTOOLS_CM_R002',
           c_const_name TYPE ztools_const_name VALUE 'TEST_SAP_RANGE_CONSTANT'.

DATA: d_developer TYPE abap_bool.

PARAMETERS: p_value TYPE i.
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN: BEGIN OF SCREEN 9001.
  SELECT-OPTIONS: s_valran FOR p_value.
SELECTION-SCREEN: END OF SCREEN 9001.

INITIALIZATION.
  PERFORM initialization.
AT SELECTION-SCREEN.
  PERFORM at_selection_screen.
START-OF-SELECTION.
  PERFORM start_of_selection.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .
* Determine if the user is a developer, constant configuration buttons
* will be shown only for developers.
* For testing purposes all users are devs.
  d_developer = abap_true.

  IF d_developer = abap_true.
    CONCATENATE icon_configuration 'Config. test constant'
    INTO sscrfields-functxt_01.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen .
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM configure_test_constant.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONFIGURE_TEST_CONSTANT
*&---------------------------------------------------------------------*
FORM configure_test_constant .
  CALL METHOD ztools_constants_manager=>read_constant
    EXPORTING
      pi_name  = c_const_name
    IMPORTING
      pe_value = s_valran[].

  CALL SELECTION-SCREEN 9001 STARTING AT 10 10 ENDING AT 100 11.

  IF sy-subrc = 0.
    CALL METHOD ztools_constants_manager=>write_constant
      EXPORTING
        pi_name  = c_const_name
        pi_group = c_const_group
        pi_type  = ztools_constants_manager=>e_constant_types-sap_range
        pi_value = s_valran[].

    MESSAGE 'Constant updated.' TYPE 'S'.
  ELSE.
    MESSAGE 'Constant not updated.' TYPE 'W'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  DATA: li_value_range TYPE RANGE OF i.

  CALL METHOD ztools_constants_manager=>read_constant
    EXPORTING
      pi_name  = c_const_name
    IMPORTING
      pe_value = li_value_range.

  IF p_value IN li_value_range.
    WRITE: / 'The value is inside the range'.
  ELSE.
    WRITE: / 'The value is not inside the range'.
  ENDIF.
ENDFORM.
