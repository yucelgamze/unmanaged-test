CLASS lhc_Student DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Student RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Student RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Student.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE Student.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Student.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Student.

    METHODS read FOR READ
      IMPORTING keys FOR READ Student RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Student.

    METHODS rba_Results FOR READ
      IMPORTING keys_rba FOR READ Student\_Results FULL result_requested RESULT result LINK association_links.

    METHODS cba_Results FOR MODIFY
      IMPORTING entities_cba FOR CREATE Student\_Results.

    METHODS earlynumbering_cba_Results FOR NUMBERING
      IMPORTING entities FOR CREATE Student\_Results.

ENDCLASS.

CLASS lhc_Student IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.

    zgy_cl_students_api_class=>get_instance( )->create_student(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

  ENDMETHOD.

  METHOD earlynumbering_create.

    zgy_cl_students_api_class=>get_instance(  )->earlynumbering_create_student(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

  ENDMETHOD.

  METHOD update.

    zgy_cl_students_api_class=>get_instance(  )->update_student(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.

    zgy_cl_students_api_class=>get_instance( )->read_student(
      EXPORTING
        keys     = keys
      CHANGING
        result   = result
        failed   = failed
        reported = reported
    ).

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD rba_Results.
  ENDMETHOD.

  METHOD cba_Results.
  ENDMETHOD.

  METHOD earlynumbering_cba_Results.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Results DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Results.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Results.

    METHODS read FOR READ
      IMPORTING keys FOR READ Results RESULT result.

    METHODS rba_Student FOR READ
      IMPORTING keys_rba FOR READ Results\_Student FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_Results IMPLEMENTATION.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Student.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZGY_I_STUDENT_UM DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZGY_I_STUDENT_UM IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    zgy_cl_students_api_class=>get_instance( )->savedata(
      CHANGING
        reported = reported
    ).

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
