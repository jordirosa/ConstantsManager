class ZTOOLS_CONSTANTS_MANAGER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF T_ACCEPTS_CREATION,
           group TYPE ztools_const_group,
           type TYPE ztools_const_type,
         END OF T_ACCEPTS_CREATION .

  constants:
    BEGIN OF E_CONSTANT_TYPES,
               NOT_EXISTS TYPE ZTOOLS_CONST_TYPE VALUE '00',
               FIELD TYPE ZTOOLS_CONST_TYPE VALUE '01',
               SAP_RANGE TYPE ZTOOLS_CONST_TYPE VALUE '02',
             END OF E_CONSTANT_TYPES .

  class-methods READ_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
    exporting
      !PE_GROUP type ZTOOLS_CONST_GROUP
      !PE_TYPE type ZTOOLS_CONST_TYPE
      !PE_VALUE type ANY .
  class-methods WRITE_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
      !PI_GROUP type ZTOOLS_CONST_GROUP optional
      !PI_TYPE type ZTOOLS_CONST_TYPE
      !PI_VALUE type ANY .
  class-methods CONFIGURE_CONSTANT
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
      !PI_ACCEPTS_CREATION type T_ACCEPTS_CREATION optional
    exporting
      !PE_SUBRC type SYSUBRC
      !PE_VALUE type ANY .
protected section.
private section.

  constants C_VERSION_1_0 type STRING value '1.0' ##NO_TEXT.
  constants:
    BEGIN OF E_XML_TAGS,
               range      TYPE string VALUE 'range',
               line       TYPE string VALUE 'line',
               sign       TYPE string VALUE 'sign',
               option     TYPE string VALUE 'option',
               low        TYPE string VALUE 'low',
               high       TYPE string VALUE 'high',
               version    TYPE string VALUE 'version',
             END OF E_XML_TAGS .

  class-methods HELPER_READ_CONST_FIELD
    importing
      !PI_VALUE type ZTOOLS_CONST_VALUE
    exporting
      !PE_VALUE type ANY .
  class-methods HELPER_WRITE_CONST_FIELD
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
      !PI_GROUP type ZTOOLS_CONST_GROUP
      !PI_VALUE type ANY .
  class-methods HELPER_CONF_CONST_FIELD
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
    exporting
      !PE_SUBRC type SYSUBRC
    changing
      !PC_VALUE type ANY .
  class-methods HELPER_READ_CONST_SAP_RANGE
    importing
      !PI_VALUE type ZTOOLS_CONST_VALUE
    exporting
      !PE_VALUE type ANY .
  class-methods HELPER_WRITE_CONST_SAP_RANGE
    importing
      !PI_NAME type ZTOOLS_CONST_NAME
      !PI_GROUP type ZTOOLS_CONST_GROUP
      !PI_VALUE type ANY .
ENDCLASS.



CLASS ZTOOLS_CONSTANTS_MANAGER IMPLEMENTATION.


  method CONFIGURE_CONSTANT.
    DATA: l_group TYPE ztools_const_group,
          l_type TYPE ztools_const_type.

    CLEAR pe_subrc.
    CLEAR pe_value.

    CALL METHOD read_constant
      EXPORTING
        pi_name  = pi_name
      IMPORTING
        pe_group = l_group
        pe_type  = l_type
        pe_value = pe_value.

    IF l_type = e_constant_types-not_exists.
      IF pi_accepts_creation IS SUPPLIED.
        l_type = pi_accepts_creation-type.
        l_group = pi_accepts_creation-group.
      ELSE.
        pe_subrc = 1.
        EXIT.
      ENDIF.
    ENDIF.

    CASE l_type.
      WHEN e_constant_types-field.
        CALL METHOD helper_conf_const_field
          EXPORTING
            pi_name  = pi_name
          IMPORTING
            pe_subrc = pe_subrc
          CHANGING
            pc_value = pe_value.
      WHEN OTHERS.
        pe_subrc = 1.
    ENDCASE.

    IF pe_subrc = 0.
      CALL METHOD write_constant
        EXPORTING
          pi_name  = pi_name
          pi_group = l_group
          pi_type  = l_type
          pi_value = pe_value.
    ENDIF.
  endmethod.


  method HELPER_CONF_CONST_FIELD.
    DATA: l_title TYPE string,
          l_value TYPE string,

          l_answer TYPE C LENGTH 1.

    CLEAR pe_subrc.
    l_value = pc_value.

    l_title = |Configuring constant { pi_name }...|.

    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname           = 'NAME'
        tabname             = 'ZTOOLS_CM_T001'
        titel               = l_title
        valuein             = l_value
      IMPORTING
        answer              = l_answer
        valueout            = l_value
      EXCEPTIONS
        fieldname_not_found = 1
        others              = 2.

    IF sy-subrc = 0 AND l_answer IS INITIAL.
      pc_value = l_value.
    ELSE.
      pe_subrc = 1.
    ENDIF.
  endmethod.


  method HELPER_READ_CONST_FIELD.
    pe_value = pi_value.
  endmethod.


  method HELPER_READ_CONST_SAP_RANGE.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE ANY,
                   <l_field> TYPE ANY.

**********************************************************************
*  Variables needed for XML macros.
   DATA: lo_xml_document TYPE REF TO cl_xml_document,

         lo_element_tmp TYPE REF TO if_ixml_element,
         lo_node_tmp TYPE REF TO if_ixml_node,
         lo_iterator_tmp TYPE REF TO if_ixml_node_iterator,

**********************************************************************
*  Other variables
         l_retcode TYPE sysubrc,

         lo_element_root TYPE REF TO if_ixml_element,
         lo_collection_lines TYPE REF TO if_ixml_node_collection,
         lo_element_line TYPE REF TO if_ixml_element.
**********************************************************************

    PARSE_XML_STRING pi_value l_retcode.

    IF l_retcode = 0.
      GET_ROOT_ELEMENT lo_element_root.

      IF lo_element_root->get_name( ) = ztools_constants_manager=>e_xml_tags-range.
        ASSIGN pe_value TO <li_table>.

        FIND_ELEMENTS lo_element_root ztools_constants_manager=>e_xml_tags-line lo_collection_lines.

        ITERATE_XML_COLLECTION lo_collection_lines lo_element_line.
          IF lo_element_line IS BOUND.
            APPEND INITIAL LINE TO <li_table> ASSIGNING <lwa_line>.

            ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line ztools_constants_manager=>e_xml_tags-sign <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line ztools_constants_manager=>e_xml_tags-option <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line ztools_constants_manager=>e_xml_tags-low <l_field>.
            ENDIF.

            ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
            IF sy-subrc = 0.
              READ_XML_FIELD lo_element_line ztools_constants_manager=>e_xml_tags-high <l_field>.
            ENDIF.
          ENDIF.
        END_ITERATE_XML_COLLECTION.
      ENDIF.
    ENDIF.
  endmethod.


  method HELPER_WRITE_CONST_FIELD.
    DATA: lwa_ztools_cm_t001 TYPE ztools_cm_t001.

    lwa_ztools_cm_t001-name = pi_name.
    lwa_ztools_cm_t001-group_name = pi_group.
    lwa_ztools_cm_t001-type = e_constant_types-field.
    lwa_ztools_cm_t001-value = pi_value.

    MODIFY ztools_cm_t001 FROM lwa_ztools_cm_t001.
  endmethod.


  method HELPER_WRITE_CONST_SAP_RANGE.
    FIELD-SYMBOLS: <li_table> TYPE STANDARD TABLE,
                   <lwa_line> TYPE ANY,
                   <l_field> TYPE ANY.

**********************************************************************
*  Variables needed for XML macros.
   DATA: lo_ixml TYPE REF TO if_ixml,
         lo_document TYPE REF TO if_ixml_document,
         lo_element_tmp TYPE REF TO if_ixml_element,

         lo_xml_document TYPE REF TO cl_xml_document,

         l_string_tmp TYPE string,
**********************************************************************
*  Other variables
         lwa_ztools_cm_t001 TYPE ztools_cm_t001,

         lo_root TYPE REF TO if_ixml_element,
         lo_line TYPE REF TO if_ixml_element.
**********************************************************************

    lwa_ztools_cm_t001-name = pi_name.
    lwa_ztools_cm_t001-group_name = pi_group.
    lwa_ztools_cm_t001-type = e_constant_types-sap_range.

    CREATE_XML_DOCUMENT.

    CREATE_XML_ELEMENT   lo_document    ztools_constants_manager=>e_xml_tags-range    lo_root.
    CREATE_XML_ATTRIBUTE lo_root        ztools_constants_manager=>e_xml_tags-version  ztools_constants_manager=>c_version_1_0.

    ASSIGN pi_value TO <li_table>.
    IF sy-subrc = 0.
      LOOP AT <li_table> ASSIGNING <lwa_line>.
        CREATE_XML_ELEMENT lo_root        ztools_constants_manager=>e_xml_tags-line     lo_line.

        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        ztools_constants_manager=>e_xml_tags-sign     <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        ztools_constants_manager=>e_xml_tags-option   <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        ztools_constants_manager=>e_xml_tags-low      <l_field>.
        ENDIF.

        ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lwa_line> TO <l_field>.
        IF sy-subrc = 0.
          WRITE_XML_FIELD  lo_line        ztools_constants_manager=>e_xml_tags-high     <l_field>.
        ENDIF.
      ENDLOOP.

      CREATE_XML_STRING lwa_ztools_cm_t001-value.
    ENDIF.

    MODIFY ztools_cm_t001 FROM lwa_ztools_cm_t001.
  endmethod.


  method READ_CONSTANT.
    DATA: lwa_ztools_cm_t001 TYPE ztools_cm_t001.

    CLEAR pe_group.
    CLEAR pe_type.
    CLEAR pe_value.

    SELECT SINGLE *
    FROM ztools_cm_t001
    INTO lwa_ztools_cm_t001
    WHERE name = pi_name.

    IF sy-subrc = 0.
      pe_group = lwa_ztools_cm_t001-group_name.
      pe_type = lwa_ztools_cm_t001-type.

      CASE lwa_ztools_cm_t001-type.
        WHEN e_constant_types-field.
          CALL METHOD helper_read_const_field
            EXPORTING
              pi_value = lwa_ztools_cm_t001-value
            IMPORTING
              pe_value = pe_value.
        WHEN e_constant_types-sap_range.
          CALL METHOD helper_read_const_sap_range
            EXPORTING
              pi_value = lwa_ztools_cm_t001-value
            IMPORTING
              pe_value = pe_value.
      ENDCASE.
    ELSE.
      pe_type = e_constant_types-not_exists.
    ENDIF.
  endmethod.


  method WRITE_CONSTANT.
    CASE pi_type.
      WHEN e_constant_types-field.
        CALL METHOD helper_write_const_field
          EXPORTING
            pi_name  = pi_name
            pi_group = pi_group
            pi_value = pi_value.
      WHEN e_constant_types-sap_range.
        CALL METHOD helper_write_const_sap_range
          EXPORTING
            pi_name  = pi_name
            pi_group = pi_group
            pi_value = pi_value.
    ENDCASE.
  endmethod.
ENDCLASS.
