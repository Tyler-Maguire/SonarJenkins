class ZHRSFCO_GENERIC_ODATAPORT_TYPE definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_GENERIC_ODATA
    importing
      !INPUT type ZHRSFGET_GENERIC_ODATA
    exporting
      !OUTPUT type ZHRSFGET_GENERIC_ODATARESPONSE
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZHRSFCO_GENERIC_ODATAPORT_TYPE IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZHRSFCO_GENERIC_ODATAPORT_TYPE'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method GET_GENERIC_ODATA.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'GET_GENERIC_ODATA'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
