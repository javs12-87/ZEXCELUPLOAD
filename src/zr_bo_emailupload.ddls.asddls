@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_BO_EMAILUPLOAD
  as select from ZBO_EMAILUPLOAD
{
  key zuuid as Zuuid,
  zbo_firstname as ZboFirstname,
  zbo_lastname as ZboLastname,
  zbo_email as ZboEmail,
  @Semantics.systemDateTime.createdAt: true
  createdat as Createdat,
  @Semantics.systemDateTime.lastChangedAt: true
  lastmodifieddatetime as Lastmodifieddatetime,
  @Semantics.user.createdBy: true
  creationuser as Creationuser,
  @Semantics.user.lastChangedBy: true
  lastmodifieduser as Lastmodifieduser
  
}
