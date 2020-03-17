class Z_PET definition
  public
  final
  create public .

public section.

  class-data NAME type STRING .
  class-data OWNER type STRING .
  class-data STATE type STRING .
  data INAME type STRING .

  class-methods CHANGEOWNER
    importing
      !LV_NEWNAME type STRING .
  class-methods PUTDOWN
    importing
      !LV_DEC type C default ' ' . " .
  methods RENAME
    importing
      !LV_NEWNAME type STRING .
protected section.
private section.
ENDCLASS.



CLASS Z_PET IMPLEMENTATION.


  method CHANGEOWNER.

owner = lv_NewName.


  endmethod.


  method PUTDOWN.

    IF lv_DEC = 'X'.
      name = 'Put Down'.
      owner = 'Put Down'.
      ENDIF.
  endmethod.


  method RENAME.

    IF lv_newname IS NOT INITIAL.

    name = lv_newname.

    ENDIF.

  endmethod.
ENDCLASS.
