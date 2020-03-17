class ZCL_Z_DELEK_US_MPC_EXT definition
  public
  inheriting from ZCL_Z_DELEK_US_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_DELEK_US_MPC_EXT IMPLEMENTATION.


  method DEFINE.


  DATA:
lo_entity   type REF TO /IWBEP/IF_MGW_ODATA_ENTITY_TYP,
lo_property type REF TO /IWBEP/IF_MGW_ODATA_PROPERTY.

lo_entity = model->GET_ENTITY_TYPE( IV_ENTITY_NAME = 'FILE' ).

IF lo_entity is BOUND.

  lo_property = lo_entity->GET_PROPERTY( IV_PROPERTY_NAME = 'ContentType' ).

  lo_property->SET_AS_CONTENT_TYPE( ).
ENDIF.

  endmethod.
ENDCLASS.
