*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
* XML writing MACROS
* Needed variables
*   DATA: lo_ixml TYPE REF TO if_ixml,
*         lo_document TYPE REF TO if_ixml_document,
*         lo_element_tmp TYPE REF TO if_ixml_element,
*
*         lo_xml_document TYPE REF TO cl_xml_document,
*
*         l_string_tmp TYPE string.
DEFINE create_xml_document.
  lo_ixml = cl_ixml=>create( ).

  lo_document = lo_ixml->create_document( ).
END-OF-DEFINITION.
DEFINE create_xml_element.
  &3 = lo_document->create_simple_element( name = &2 parent = &1 ).
END-OF-DEFINITION.
DEFINE create_xml_attribute.
  &1->set_attribute( name = &2 value = &3 ).
END-OF-DEFINITION.
DEFINE write_xml_field.
  IF NOT &3 IS INITIAL.
    l_string_tmp = &3.

    lo_element_tmp = lo_document->create_simple_element( name = &2 value = l_string_tmp parent = &1 ).
  ENDIF.
END-OF-DEFINITION.
DEFINE create_xml_string.
  CREATE OBJECT lo_xml_document.

  CALL METHOD lo_xml_document->create_with_dom
    EXPORTING
      document = lo_document.

  CALL METHOD lo_xml_document->render_2_string
    IMPORTING
      stream       = &1.
END-OF-DEFINITION.
* XML reading MACROS
* Needed variables
*   DATA: lo_xml_document TYPE REF TO cl_xml_document,
*
*         lo_element_tmp TYPE REF TO if_ixml_element,
*         lo_node_tmp TYPE REF TO if_ixml_node,
*         lo_iterator_tmp TYPE REF TO if_ixml_node_iterator.
DEFINE parse_xml_string.
  CREATE OBJECT lo_xml_document.

  CALL METHOD lo_xml_document->parse_string
    EXPORTING
      stream  = &1
    RECEIVING
      retcode = &2.
END-OF-DEFINITION.
DEFINE get_root_element.
      CALL METHOD lo_xml_document->m_document->get_root_element
        RECEIVING
          RVAL   = &1.
END-OF-DEFINITION.
DEFINE find_element.
  CALL METHOD &1->find_from_name_ns
    EXPORTING
      DEPTH  = 2
      NAME   = &2
    RECEIVING
      RVAL   = &3.
END-OF-DEFINITION.
DEFINE find_elements.
  CALL METHOD &1->get_elements_by_tag_name
    EXPORTING
      DEPTH  = 2
      NAME      = &2
    RECEIVING
      RVAL      = &3.
END-OF-DEFINITION.
DEFINE iterate_xml_collection.
  CALL METHOD &1->create_iterator
    RECEIVING
      rval = lo_iterator_tmp.

  lo_node_tmp = lo_iterator_tmp->get_next( ).
  WHILE lo_node_tmp IS BOUND.
    &2 ?= lo_node_tmp->if_ixml_unknown~query_interface( ixml_iid_element ).
END-OF-DEFINITION.
DEFINE end_iterate_xml_collection.
    lo_node_tmp = lo_iterator_tmp->get_next( ).
  ENDWHILE.
END-OF-DEFINITION.
DEFINE read_xml_field.
  CALL METHOD &1->find_from_name_ns
    EXPORTING
      DEPTH  = 2
      NAME   = &2
    RECEIVING
      RVAL   = lo_element_tmp.

  IF lo_element_tmp IS BOUND.
    &3 = lo_element_tmp->get_value( ).
  ELSE.
    CLEAR &3.
  ENDIF.
END-OF-DEFINITION.
