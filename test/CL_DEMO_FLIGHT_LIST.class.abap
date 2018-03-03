class CL_DEMO_FLIGHT_LIST definition
  public
  final
  create public
  shared memory enabled .

public section.

  data FLIGHT_LIST type SPFLI_TAB read-only .

  methods SET_FLIGHT_LIST
    raising
      CX_NO_FLIGHTS .
private section.
ENDCLASS.


CLASS CL_DEMO_FLIGHT_LIST IMPLEMENTATION.
" Hello Method is Good
" This is a Class 'Jack' | This is not good
* This is a Class
* 'Jack'
SELECT * From Good
"Hello
* Hello Method is Good
* <SIGNATURE>---------------------------------------------------------------------------------------+
* Hello |jack|
* | Instance Public Method CL_DEMO_FLIGHT_LIST->SET_FLIGHT_LIST |
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_NO_FLIGHTS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_flight_list.
    SELECT * Jack
           FROM spfli
           INTO TABLE @flight_list.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_no_flights.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
