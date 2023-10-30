@EndUserText.label: 'Student Consumption View Unmanaged'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity zgy_c_student_um
  as projection on zgy_i_student_um
{ @EndUserText.label: 'ID'
  key Id,
  @EndUserText.label: 'Student ID'
      Studentid,
      @EndUserText.label: 'First Name'
      Firstname,
      @EndUserText.label: 'Last Name'
      Lastname,
       @EndUserText.label: 'Student Age'
      Studentage,
       @EndUserText.label: 'Course'
      Course,
      @EndUserText.label: 'Course Duration'
      Courseduration,
       @EndUserText.label: 'Student Status'
      Studentstatus,
       @EndUserText.label: 'Gender'
      Gender,
      Genderdesc,
      @EndUserText.label: 'Date of Birth'
      Dob,
//      Lastchangedat,
//      Locallastchangedat,
      
      /* Associations */
      _gender,
      _results: redirected to composition child zgy_c_results_um
}
