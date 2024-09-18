@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_BO_EMAILUPLOAD
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_BO_EMAILUPLOAD
{
  key Zuuid,
  ZboFirstname,
  ZboLastname,
  ZboEmail,
  Createdat,
  Lastmodifieddatetime,
  Creationuser,
  Lastmodifieduser
  
}
