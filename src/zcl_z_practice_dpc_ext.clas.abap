class ZCL_Z_PRACTICE_DPC_EXT definition
  public
  inheriting from ZCL_Z_PRACTICE_DPC
  create public .

public section.
protected section.

  methods POSITIONSSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_PRACTICE_DPC_EXT IMPLEMENTATION.


  method POSITIONSSET_GET_ENTITYSET.

SELECT * FROM HRP1000 INTO CORRESPONDING FIELDS OF TABLE et_entityset WHERE otype = 'S'.

  endmethod.
ENDCLASS.
