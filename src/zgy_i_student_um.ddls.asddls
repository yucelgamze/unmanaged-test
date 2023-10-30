@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Students Interface View Unmanaged'
define root view entity zgy_i_student_um
  as select from zgy_student_um
  association [1..*] to zgy_i_gender_um  as _gender on $projection.Gender = _gender.value
  composition [0..*] of zgy_i_results_um as _results
{
  key id                  as Id,
      studentid           as Studentid,
      firstname           as Firstname,
      lastname            as Lastname,
      studentage          as Studentage,
      course              as Course,
      courseduration      as Courseduration,
      studentstatus       as Studentstatus,
      gender              as Gender,
      dob                 as Dob,
      lastchangedat       as Lastchangedat,
      locallastchangedat  as Locallastchangedat,
      _gender.Description as Genderdesc,
      _gender,
      _results
}
