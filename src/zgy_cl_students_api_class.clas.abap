CLASS zgy_cl_students_api_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_create_student TYPE TABLE FOR CREATE zgy_i_student_um\\student,
      tt_mapped_early   TYPE RESPONSE FOR MAPPED EARLY zgy_i_student_um,
      tt_failed_early   TYPE RESPONSE FOR FAILED EARLY zgy_i_student_um,
      tt_reported_early TYPE RESPONSE FOR REPORTED EARLY zgy_i_student_um,
      tt_reported_late  TYPE RESPONSE FOR REPORTED LATE zgy_i_student_um,

      tt_student_keys   TYPE TABLE FOR READ IMPORT zgy_i_student_um\\student,
      tt_student_result TYPE TABLE FOR READ RESULT zgy_i_student_um\\student,

      tt_update_student TYPE TABLE FOR UPDATE zgy_i_student_um\\student.

    "Create constructor

    CLASS-METHODS: get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zgy_cl_students_api_class.

    "Class Methods

    METHODS:
      earlynumbering_create_student
        IMPORTING entities TYPE tt_create_student  "table for CREATE zgy_i_student_um\\student
        CHANGING  mapped   TYPE tt_mapped_early    "response for mapped early zgy_i_student_um
                  failed   TYPE tt_failed_early    "response for failed early zgy_i_student_um
                  reported TYPE tt_reported_early, "response for reported early zgy_i_student_um

      create_student
        IMPORTING entities TYPE tt_create_student "table for CREATE zgy_i_student_um\\student
        CHANGING  mapped   TYPE tt_mapped_early   "response for mapped early zgy_i_student_um
                  failed   TYPE tt_failed_early   "response for failed early zgy_i_student_um
                  reported TYPE tt_reported_early,

      get_next_id
        RETURNING
          VALUE(rv_id) TYPE sysuuid_x16,

      get_next_student_id
        RETURNING
          VALUE(rv_studentid) TYPE zgy_de_student_id_um,

      savedata
        CHANGING reported TYPE tt_reported_late,"response for reported late zgy_i_student_um

      read_student
        IMPORTING keys     TYPE tt_student_keys   "table for read import zgy_i_student_um\\student
        CHANGING  result   TYPE tt_student_result "table for read result zgy_i_student_um\\student
                  failed   TYPE tt_failed_early   "response for failed early zgy_i_student_um
                  reported TYPE tt_reported_early,"response for reported early zgy_i_student_um

      update_student
        IMPORTING entities TYPE tt_update_student"table for UPDATE zgy_i_student_um\\student
        CHANGING  mapped   TYPE tt_mapped_early"response for mapped early zgy_i_student_um
                  failed   TYPE tt_failed_early "response for failed early zgy_i_student_um
                  reported TYPE tt_reported_early."response for reported early zgy_i_student_um

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: mo_instance TYPE REF TO zgy_cl_students_api_class,
                gt_student  TYPE STANDARD TABLE OF zgy_student_um,
                gt_results  TYPE STANDARD TABLE OF zgy_results_um,
                gs_mapped   TYPE tt_mapped_early.

ENDCLASS.



CLASS zgy_cl_students_api_class IMPLEMENTATION.
  METHOD get_instance.

    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                       THEN mo_instance
                                       ELSE NEW #(  ) ).

  ENDMETHOD.

  METHOD earlynumbering_create_student.

    DATA(ls_mapped) = gs_mapped.
    "cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16(  ).
    DATA(lv_new_id) = get_next_id( ).

    "Buffer Table Update
    READ TABLE gt_student ASSIGNING FIELD-SYMBOL(<lfs_student>) INDEX 1.
    IF <lfs_student> IS ASSIGNED.
      <lfs_student>-id  = lv_new_id.
      UNASSIGN <lfs_student>.
    ENDIF.

    "backend'deki datayı frontend'e göndermek için:
    mapped-student = VALUE #(
       FOR ls_entities IN entities WHERE ( Id IS INITIAL )
       (
            %cid = ls_entities-%cid
            %is_draft = ls_entities-%is_draft
            Id = lv_new_id
       )
    ).

  ENDMETHOD.

  METHOD create_student.
    "frontend'deki veriyi buffer table'a aktarıyor
    gt_student = CORRESPONDING #( entities MAPPING FROM ENTITY ).

    IF NOT gt_student[] IS INITIAL.
      gt_student[ 1 ]-studentid = get_next_student_id( ).
    ENDIF.

    mapped = VALUE #(
    student = VALUE #(
              FOR ls_entity IN entities (
        %cid = ls_entity-%cid
        %key = ls_entity-%key
        %is_draft = ls_entity-%is_draft )
              )
   ).
*    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
*      IF NOT gt_student[] IS INITIAL.
*        gt_student[ 1 ]-studentid = get_next_student_id( ).
*
*        mapped-student = VALUE #(
*   (
*        %cid = <lfs_entities>-%cid
*        %key = <lfs_entities>-%key
*        %is_draft = <lfs_entities>-%is_draft
*   )
*).
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_next_id.
    TRY.
        rv_id = cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16(  ).
      CATCH cx_uuid_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_next_student_id.
    SELECT MAX( studentid ) FROM zgy_student_um INTO @DATA(lv_max_studentid).
    rv_studentid = lv_max_studentid + 1.
  ENDMETHOD.

  METHOD savedata.
    IF NOT gt_student[] IS INITIAL.
      MODIFY zgy_student_um FROM TABLE @gt_student.
    ENDIF.
  ENDMETHOD.

  METHOD read_student.

    SELECT * FROM zgy_student_um FOR ALL ENTRIES IN @keys
    WHERE id = @keys-Id
    INTO TABLE @DATA(lt_student_data).

    result = CORRESPONDING #( lt_student_data MAPPING TO ENTITY ).

  ENDMETHOD.

  METHOD update_student.

    DATA: lt_student_update   TYPE STANDARD TABLE OF zgy_student_um,
          lt_student_update_x TYPE STANDARD TABLE OF zgy_cs_student_prop_um.

    "read from entity and putting it to the local table so we use mapping from entity
    lt_student_update   = CORRESPONDING #( entities MAPPING FROM ENTITY ).
    lt_student_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    IF NOT lt_student_update IS INITIAL.

      SELECT * FROM zgy_student_um
      FOR ALL ENTRIES IN @lt_student_update
      WHERE id = @lt_student_update-id
      INTO TABLE @DATA(lt_student_update_old).

    ENDIF.

    gt_student = VALUE #(

    FOR x = 1 WHILE x <= lines( lt_student_update )

    LET
       ls_control_flag = VALUE #( lt_student_update_x[ x ] OPTIONAL )
       ls_student_new  = VALUE #( lt_student_update[ x ] OPTIONAL )
       ls_student_old  = VALUE #( lt_student_update_old[ id = ls_student_new-id ] OPTIONAL )
    IN
    (
        id = ls_student_new-id
        studentid      = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-studentid ELSE ls_student_old-studentid )
        firstname      = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-firstname ELSE ls_student_old-firstname )
        lastname       = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-lastname ELSE ls_student_old-lastname )
        studentage     = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-studentage ELSE ls_student_old-studentage )
        course         = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-course ELSE ls_student_old-course )
        courseduration = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-courseduration ELSE ls_student_old-courseduration )
        studentstatus  = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-studentstatus ELSE ls_student_old-studentstatus )
        gender         = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-gender ELSE ls_student_old-gender )
        dob            = COND #( WHEN ls_control_flag IS NOT INITIAL THEN ls_student_new-dob ELSE ls_student_old-dob )
     )
     ).

  ENDMETHOD.

ENDCLASS.
