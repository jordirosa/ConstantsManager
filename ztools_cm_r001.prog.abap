*&---------------------------------------------------------------------*
*& Report ZTOOLS_CM_R001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTOOLS_CM_R001.

TABLES: sscrfields.

CONSTANTS: c_const_group TYPE ztools_const_group VALUE 'ZTOOLS_CM_R001',
           c_const_name TYPE ztools_const_name VALUE 'TEST_FIELD_CONSTANT'.

DATA: d_developer TYPE abap_bool.

PARAMETERS: p_value TYPE i.
SELECTION-SCREEN FUNCTION KEY 1.

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
  DATA: l_subrc TYPE sysubrc,

        lwa_accepts_creation TYPE ztools_constants_manager=>t_accepts_creation.

  lwa_accepts_creation-group = c_const_group.
  lwa_accepts_creation-type = ztools_constants_manager=>e_constant_types-field.

  CALL METHOD ztools_constants_manager=>configure_constant
    EXPORTING
      pi_name             = c_const_name
      pi_accepts_creation = lwa_accepts_creation
    IMPORTING
      pe_subrc            = l_subrc.

  IF l_subrc = 0.
    MESSAGE 'Constant updated.' TYPE 'S'.
  ELSE.
    MESSAGE 'Constant not updated.' TYPE 'W'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .
  DATA: l_value TYPE i,
        l_total TYPE i.

  CALL METHOD ztools_constants_manager=>read_constant
    EXPORTING
      pi_name  = c_const_name
    IMPORTING
      pe_value = l_value.

  l_total = l_value + p_value.

  WRITE: / 'Constant value =', l_value.
  WRITE: / 'Sum =', l_total.
ENDFORM.
