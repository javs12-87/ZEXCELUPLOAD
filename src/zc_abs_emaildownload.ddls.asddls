@EndUserText.label: 'Abstract Email Entity'
define abstract entity ZC_ABS_EMAILDOWNLOAD
{
  fileContent   : abap.string(0);
  fileName      : abap.string(0);
  fileExtension : abap.char( 64 );
  mimeType      : abap.string(0);
}
