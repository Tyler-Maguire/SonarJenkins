class Z_PERSON definition
  public
  final
  create public .

public section.

  class-data LV_NAME type STRING .
  class-data LV_ANSWER type I .

  class-methods GET_NAME
    importing
      !LV_FIRSTNAME type STRING
      !LV_LASTNAME type STRING
    exporting
      !LV_NAME type STRING
    exceptions
      EMPTY_NAME .
  methods SUBTRAC
    importing
      !INPUT1 type I
      !INPUT2 type I
    exporting
      !ANSWER type I .
protected section.
private section.
ENDCLASS.



CLASS Z_PERSON IMPLEMENTATION.


  method GET_NAME.

DATA: WhiteSPACE TYPE STRING.

WhiteSPACE = ' '.

CONCATENATE LV_FIRSTNAME WhiteSPACE LV_LASTNAME INTO LV_NAME.

WRITE: 'Please Remeber to Request FullName for Formal Programs.'.


  endmethod.


  method SUBTRAC.

ANSWER = INPUT1 - INPUT2.

  endmethod.
ENDCLASS.
