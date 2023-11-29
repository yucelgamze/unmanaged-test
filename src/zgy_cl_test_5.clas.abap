CLASS zgy_cl_test_5 DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES:
      td_key TYPE c LENGTH 5,

      BEGIN OF ts_simple_data,
        tkey   TYPE td_key,
        text   TYPE string,
        number TYPE i,
        date   TYPE d,
      END OF ts_simple_data,
      tt_simple_data TYPE STANDARD TABLE OF ts_simple_data WITH EMPTY KEY,

      BEGIN OF ts_easy_data,
        tkey   TYPE td_key,
        text   TYPE string,
        number TYPE i,
      END OF ts_easy_data,
      tt_easy_data TYPE STANDARD TABLE OF ts_easy_data WITH EMPTY KEY,

      tt_numbers   TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      generate_data
        RETURNING VALUE(rt_result) TYPE tt_simple_data,

      simple_for_with_counter
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,

      simple_for_with_mapping
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,

      for_with_reduce
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,

      for_with_where_condition
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out,
      simple_for_with_date
        IMPORTING
          io_out TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.


CLASS zgy_cl_test_5 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    simple_for_with_counter( out ).
    simple_for_with_date( out ).
    simple_for_with_mapping( out ).
    for_with_reduce( out ).
    for_with_where_condition( out ).
  ENDMETHOD.


  METHOD generate_data.
    rt_result = VALUE #(
      ( tkey = 'A' text = `Banana` number = 14 date = '20230101' )
      ( tkey = 'B' text = `Tomato` number = 12 date = '20230201' )
      ( tkey = 'C' text = `Apple` number = 23 date = '20231201' )
      ( tkey = 'E' text = `Strawberry` number = 31 date = '20230101' )
      ( tkey = 'F' text = `Salad` number = 20 date = '20230601' )
    ).
  ENDMETHOD.


  METHOD simple_for_with_counter.
    DATA(lt_numbers_until) = VALUE tt_numbers(
      FOR i = 1 UNTIL i >= 10 ( i )
    ).

    io_out->write( `Result table with NUMBERS (UNTIL):` ).
    io_out->write( lt_numbers_until ).

    DATA(lt_numbers_while) = VALUE tt_numbers(
      FOR i = 1 WHILE i <= 10 ( i )
    ).

    io_out->write( `Result table with NUMBERS (WHILE):` ).
    io_out->write( lt_numbers_while ).

    DATA(lt_numbers_then) = VALUE tt_numbers(
      FOR i = 1 THEN i + 3 WHILE i <= 10 ( i )
    ).

    io_out->write( `Result table with NUMBERS (THEN):` ).
    io_out->write( lt_numbers_then ).
  ENDMETHOD.


  METHOD simple_for_with_mapping.
    DATA(lt_base) = generate_data( ).

    DATA(lt_corresponding) = VALUE tt_easy_data(
      FOR ls_base IN lt_base ( CORRESPONDING #( ls_base ) )
    ).

    io_out->write( `Result table with CORRESPONDING:` ).
    io_out->write( lt_corresponding ).

    DATA(lt_simple_mapping) = VALUE tt_easy_data(
      FOR ls_base IN lt_base ( tkey = ls_base-tkey text = ls_base-text number = 1 )
    ).

    io_out->write( `Result table with simple mapping:` ).
    io_out->write( lt_simple_mapping ).
  ENDMETHOD.


  METHOD for_with_reduce.
    DATA(lt_base) = generate_data( ).

    DATA(ld_number) = REDUCE i(
      INIT num = 0
      FOR i = 1 THEN i + 3 WHILE i <= 10
      NEXT num = num + ( i * i )
    ).

    io_out->write( |Result from table reduce: { ld_number }| ).

    DATA(ld_sum) = REDUCE i(
      INIT sum = 0
      FOR ls_base IN lt_base
      NEXT sum = sum + ls_base-number
    ).

    io_out->write( |Sum of all items: { ld_sum }| ).

    DATA(ld_keys) = REDUCE string(
      INIT keys = ``
      FOR ls_base IN lt_base
      NEXT keys = keys && ls_base-tkey
    ).

    io_out->write( |All keys concatenated: { ld_keys }| ).
  ENDMETHOD.


  METHOD for_with_where_condition.
    DATA(lt_base) = generate_data( ).

    DATA(lt_where) = VALUE tt_easy_data(
      FOR ls_base IN lt_base INDEX INTO ld_idx WHERE ( date > '20230101' )
      ( tkey = ls_base-tkey text = ls_base-text number = ld_idx )
    ).

    io_out->write( `Result with date greater than 01/01/2023:` ).
    io_out->write( lt_where ).
  ENDMETHOD.


  METHOD simple_for_with_date.
    DATA(lt_date_variable) = VALUE tt_simple_data(
      FOR i = CONV d( '20230101' ) THEN i + 2 WHILE i <= '20230201' ( date = i )
    ).

    io_out->write( `Result table with date type:` ).
    io_out->write( lt_date_variable ).
  ENDMETHOD.
ENDCLASS.
