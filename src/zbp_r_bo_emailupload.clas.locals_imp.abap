CLASS lhc_zr_bo_emailupload DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    TYPES lty_failed  TYPE TABLE FOR FAILED zr_bo_emailupload.
    TYPES lty_reported TYPE TABLE FOR REPORTED zr_bo_emailupload.
    TYPES lt_file_data TYPE zbo_emailupload.

    TYPES: BEGIN OF lty_file_template,
             firstname TYPE string,
             lastname  TYPE string,
             email     TYPE string,
           END OF lty_file_template.

    TYPES: BEGIN OF lty_file,
             uuid      TYPE string,
             firstname TYPE string,
             lastname  TYPE string,
             email     TYPE string,
           END OF lty_file.

    METHODS:

      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrBoEmailupload
        RESULT result,
      downloadFile FOR READ
        IMPORTING keys FOR FUNCTION ZrBoEmailupload~downloadFile RESULT result.

    METHODS fileUpload FOR MODIFY
      IMPORTING keys FOR ACTION ZrBoEmailupload~fileUpload.

    METHODS validate_filedata
      IMPORTING iv_data_string TYPE lty_file
                iv_row_cont    TYPE sy-tabix
      EXPORTING
                et_failed      TYPE lty_failed
                et_reported    TYPE lty_reported
                ev_error       TYPE abap_boolean
                es_data        TYPE lt_file_data.

ENDCLASS.

CLASS lhc_zr_bo_emailupload IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.


  METHOD validate_filedata.

    SELECT zuuid FROM zbo_emailupload WHERE zbo_email = @iv_data_string-email INTO @DATA(lv_uuid).
    ENDSELECT.

    IF sy-subrc = 0.
      es_data-zuuid = lv_uuid.
    ELSE.

      TRY.
          DATA(lv_temp) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).

        CATCH cx_uuid_error.
      ENDTRY.

      es_data-zuuid = lv_temp.

    ENDIF.

    es_data-zbo_firstname = iv_data_string-firstname.
    es_data-zbo_lastname = iv_data_string-lastname.
    es_data-zbo_email = iv_data_string-email.

  ENDMETHOD.


  METHOD downloadFile.

    CONSTANTS: lc_fileextension TYPE char64 VALUE 'XLSX'.
    DATA: lt_template        TYPE TABLE OF zst_email_dttmpl,
          lt_components_tmpl TYPE abap_component_tab,
          lr_template_str    TYPE REF TO data.


    FIELD-SYMBOLS: <lt_tmpl_str_tab> TYPE table.

    " Get the data reference of the template table
    DATA(lr_template) = REF #( lt_template ).

    " Get table and structure description of the template table
    DATA(lr_template_table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( lr_template ) ).
    DATA(lr_template_row_descr) = CAST cl_abap_structdescr( lr_template_table_descr->get_table_line_type( ) ).

    " Get the list of components/columns of the template table
    DATA(lt_components) = lr_template_row_descr->get_components(  ).

*****************************************************************************************************************
*    Loop through the components table and create a dynamic internal table of same columns of string data type. *
*    This is needed, so columns name can be populated from Data element field labels.                           *
*****************************************************************************************************************
    IF lt_components IS NOT INITIAL.
      LOOP AT lt_components REFERENCE INTO DATA(lr_component).

        " Create a type description of string data type
        DATA(lr_typedescr_tmpl) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( CONV string( abap_false ) ) ).

        " Create a component table containing same name and number of columns but string data types.
        DATA(ls_compdescr_tmpl) = CORRESPONDING abap_componentdescr( lr_component->* EXCEPT type ).
        ls_compdescr_tmpl-type = lr_typedescr_tmpl.
        APPEND ls_compdescr_tmpl TO lt_components_tmpl.
        CLEAR ls_compdescr_tmpl.

      ENDLOOP.

      " Create structure and table description of string table
      DATA(lr_tmpl_row_descr_str) = CAST cl_abap_datadescr( cl_abap_structdescr=>create( lt_components_tmpl ) ).
      DATA(lr_tmpl_tab_descr_str) = cl_abap_tabledescr=>create( lr_tmpl_row_descr_str ).

      CREATE DATA lr_template_str TYPE HANDLE lr_tmpl_tab_descr_str.

      ASSIGN lr_template_str->* TO <lt_tmpl_str_tab>.

      APPEND INITIAL LINE TO <lt_tmpl_str_tab> ASSIGNING FIELD-SYMBOL(<ls_tmpl_str_row>).

      " Loop through the components of the string table and poulate the value based on the field labels maintained at data elements
      LOOP AT lt_components_tmpl REFERENCE INTO DATA(lr_component_tmpl).
        READ TABLE lt_components REFERENCE INTO lr_component WITH KEY name = lr_component_tmpl->name.
        IF sy-subrc IS INITIAL.
          DATA(lr_elemdescr) = CAST cl_abap_elemdescr( lr_component->type ).

**-->Begin of change: To capture traslatable text of the columns part of download template
*
*****************************************************************************************************************
**          Below section of the code retrieves translatable text of the data element                           *
*****************************************************************************************************************
          " Get the target Data element to read translated text
          DATA(lo_target) = xco_cp_i18n=>target->data_element->object( CONV sxco_ad_object_name( lr_elemdescr->help_id ) ).

          " Get the target language based on logon language
          DATA(lo_target_language) = xco_cp=>language( sy-langu ).

          " Get the data element text attribute
          DATA(lo_txt_hdg_attribute) = xco_cp_data_element=>text_attribute->heading_field_label. " Get heading field label
          DATA(lo_txt_shrt_attribute) = xco_cp_data_element=>text_attribute->short_field_label.  " Get short field label
          DATA(lo_txt_mdm_attribute)  = xco_cp_data_element=>text_attribute->medium_field_label. " Get medium field label
          DATA(lo_txt_long_attribute) = xco_cp_data_element=>text_attribute->long_field_label.   " Get long field label

          IF lo_target->has_translation( io_language = lo_target_language
                                      io_text_attribute = lo_txt_hdg_attribute
                                      ) = abap_true.
            " Read the translated text
            DATA(lo_translation) = lo_target->get_translation( io_language = lo_target_language
                                                               it_text_attributes = VALUE #( ( lo_txt_hdg_attribute ) )
                                                               ).



            LOOP AT lo_translation->texts INTO DATA(lo_text).
              " Value of the component/column
              ASSIGN COMPONENT lr_component->name OF STRUCTURE <ls_tmpl_str_row> TO FIELD-SYMBOL(<ls_comp_val>).
              IF <ls_comp_val> IS ASSIGNED.
                " Get the translated text of the data element
                <ls_comp_val> = lo_txt_hdg_attribute->if_xco_i18n_text_attribute~get_string_for_text( lo_text->value ).
              ENDIF.
            ENDLOOP.
          ENDIF.
**<--End of change: To capture traslatable text of the columns part of download template
        ENDIF.
      ENDLOOP.

    ENDIF.


    " Creating a new empty XLSX document obtaining write access.
    DATA(lo_write_access) = xco_cp_xlsx=>document->empty( )->write_access( ).

    " Create an empty XLSX document consists of one worksheet named Sheet1 which is accessible at position 1, i.e. the first worksheet in the workbook.
    DATA(lo_worksheet) = lo_write_access->get_workbook(
      )->worksheet->at_position( 1 ).

    " A selection pattern that was obtained via XCO_CP_XLSX_SELECTION=>PATTERN_BUILDER using coordinates.
    " Here, we are creating the Excel template to upload mass recprds. So, only the first row is needed to create the header record
    DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
                                )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
                                )->to_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'C' )
                                )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( 1 )  " Represents header record containing column names.
                                )->get_pattern( ).

    " At this point, the internal table LT_DATA_ROWS will have been written into the
    " worksheet selection.
    lo_worksheet->select( lo_selection_pattern )->row_stream( )->operation->write_from( REF #( <lt_tmpl_str_tab> ) )->execute( ).

    " Get the file content of the document as an xstring.
    DATA(lv_file_content) = lo_write_access->get_file_content( ).

    " LV_BASE64_ENCODING is of type STRING and contains the Base64 encoded version
    " of LV_XSTRING.
    DATA(lv_base64_encoding) = xco_cp=>xstring( lv_file_content )->as_string( xco_cp_binary=>text_encoding->base64 )->value.

    result = VALUE #( ( %param-fileContent   = lv_base64_encoding
                        %param-fileName      = 'Template'
                        %param-fileExtension = 'xlsx'
                        %param-mimeType      = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' ) ).

  ENDMETHOD.

  METHOD fileUpload.

    CONSTANTS: lc_msg_class TYPE sy-msgid VALUE 'ZN5_BTP_MSG_CLS',
               lc_msg_no10  TYPE sy-msgno VALUE '010',
               lc_msg_no    TYPE sy-msgno VALUE '009',
               lc_msg_011   TYPE sy-msgno VALUE '011',
               lc_msg_013   TYPE sy-msgno VALUE '013',
               lc_char      TYPE string VALUE 'CHAR',
               lc_phone     TYPE i VALUE 10.

    DATA: lt_email_table TYPE STANDARD TABLE OF lty_file_template.

    DATA: ls_data        TYPE zbo_emailupload,
          lt_data        TYPE TABLE FOR CREATE zr_bo_emailupload,
          lt_data_update TYPE TABLE FOR UPDATE zr_bo_emailupload,
          lt_data_final  TYPE STANDARD TABLE OF zbo_emailupload,
          lt_data_create TYPE STANDARD TABLE OF zbo_emailupload,
          lt_update      TYPE STANDARD TABLE OF zbo_emailupload.
*          ls_data_string TYPE lty_file.

    DATA: lv_count   TYPE i,
          lv_xstring TYPE xstring,
          lv_error   TYPE abap_boolean.


    READ TABLE keys ASSIGNING FIELD-SYMBOL(<ls_keys>) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_filecontent) = <ls_keys>-%param-fileContent .

      " LV_FILE_CONTENT must be populated with the complete file content of the .XLSX file
      " whose content shall be processed programmatically.
      DATA(lo_read_access) = xco_cp_xlsx=>document->for_file_content( lv_filecontent )->read_access( ).

      " Read access for the worksheet at position 1, i.e. the first worksheet in the workbook.
      DATA(lo_worksheet) = lo_read_access->get_workbook(
        )->worksheet->at_position( 1 ).

      " A selection pattern that was obtained via XCO_CP_XLSX_SELECTION=>PATTERN_BUILDER using coordinates
      DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
                                  )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
                                  )->to_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'C' )
                                  )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( 2 )
                                  )->get_pattern( ).

      " At this point, the internal table LT_DATA_ROWS will contain the rows from the worksheet
      " selection.
      lo_worksheet->select( lo_selection_pattern )->row_stream( )->operation->write_to( REF #( lt_email_table ) )->if_xco_xlsx_ra_operation~execute( ).

      DELETE lt_email_table WHERE firstname IS INITIAL AND
                                lastname IS INITIAL AND
                                email IS INITIAL.

      LOOP AT lt_email_table ASSIGNING FIELD-SYMBOL(<ls_data_row>).

        " Get the row count
        DATA(lv_row_count) = sy-tabix.

        "Populate the data string for validation
        DATA(ls_data_string) = CORRESPONDING lty_file( <ls_data_row> ).

        ls_data_string-firstname = <ls_data_row>-firstname.
        ls_data_string-lastname = <ls_data_row>-lastname.
        ls_data_string-email = <ls_data_row>-email.

        "remove extra spaces from the file
        CONDENSE: ls_data_string-firstname,
                  ls_data_string-lastname,
                  ls_data_string-email.

        "Validations of item fields

        CALL METHOD validate_filedata
          EXPORTING
            iv_data_string = ls_data_string
            iv_row_cont    = lv_row_count
          IMPORTING
            et_failed      = failed-zrboemailupload
            et_reported    = reported-zrboemailupload
            es_data        = ls_data
            ev_error       = lv_error.

        IF lv_error = abap_false.
          APPEND ls_data TO lt_data_final.
        ENDIF.

        CLEAR lv_error.

      ENDLOOP.

      IF lt_data_final IS NOT INITIAL.
        SELECT FROM zbo_emailupload AS mailtabl
        INNER JOIN @lt_data_final AS data ON ##ITAB_KEY_IN_SELECT
        data~zuuid = mailtabl~zuuid
*        data~zbo_firstname = mailtabl~zbo_firstname AND
*        data~zbo_lastname = mailtabl~zbo_lastname AND
*        data~zbo_email = mailtabl~zbo_email
        FIELDS mailtabl~zuuid,
               mailtabl~zbo_firstname,
               mailtabl~zbo_lastname,
               mailtabl~zbo_email
        INTO TABLE @DATA(lt_tmp_data).

        IF sy-subrc = 0. "UPDATE RECORDS

          SORT lt_tmp_data BY zbo_firstname zbo_lastname zbo_email.

          "Check whether excel file contains records to create and update together
          LOOP AT lt_data_final ASSIGNING FIELD-SYMBOL(<ls_data_final>).
            READ TABLE lt_tmp_data ASSIGNING FIELD-SYMBOL(<ls_intemail>) WITH KEY zuuid = <ls_data_final>-zuuid BINARY SEARCH.
            IF sy-subrc <> 0. "create records

              APPEND <ls_data_final> TO lt_data_create.
            ELSE.
              APPEND <ls_data_final> TO lt_update.
            ENDIF.
          ENDLOOP.

          IF lt_data_create IS NOT INITIAL.
            lt_data = VALUE #( FOR ls_data_final IN lt_data_create (
                               zuuid                = ls_data_final-zuuid
                               zbofirstname         = ls_data_final-zbo_firstname
                               zbolastname          = ls_data_final-zbo_lastname
                               zboemail             = ls_data_final-zbo_email
                               createdAt            = ls_data_final-createdat
                               lastModifiedDateTime = ls_data_final-lastmodifieddatetime
                               creationUser         = ls_data_final-creationuser
                               lastModifiedUser     = ls_data_final-lastmodifieduser
                               %control             =
                               VALUE #(
                                        zuuid                 = if_abap_behv=>mk-on
                                        zbofirstname          = if_abap_behv=>mk-on
                                        createdAt            = if_abap_behv=>mk-off
                                        creationUser         = if_abap_behv=>mk-off
                                        zboemail                = if_abap_behv=>mk-on
                                        zbolastname           = if_abap_behv=>mk-on
                                        lastModifiedDateTime = if_abap_behv=>mk-off
                                        lastModifiedUser     = if_abap_behv=>mk-off )
                               ) ).

            MODIFY ENTITIES OF zr_bo_emailupload IN LOCAL MODE
            ENTITY zrboemailupload
            CREATE AUTO FILL CID FIELDS ( Zuuid ZboFirstname ZboLastname ZboEmail Createdat Lastmodifieddatetime Creationuser Lastmodifieduser ) WITH lt_data
            MAPPED DATA(lt_mapped_create)
            REPORTED DATA(lt_reported_create)
            FAILED DATA(lt_failed_create).

            APPEND LINES OF : lt_reported_create-zrboemailupload TO reported-zrboemailupload ,
                              lt_failed_create-zrboemailupload  TO failed-zrboemailupload ,
                              lt_mapped_create-zrboemailupload  TO mapped-zrboemailupload .
          ENDIF.

          IF lt_update IS NOT INITIAL.

            lt_data_update = VALUE #( FOR ls_data_final IN lt_update (
                               zuuid                = ls_data_final-zuuid
                               zbofirstname         = ls_data_final-zbo_firstname
                               zbolastname          = ls_data_final-zbo_lastname
                               zboemail             = ls_data_final-zbo_email
                               createdAt            = ls_data_final-createdat
                               lastModifiedDateTime = ls_data_final-lastmodifieddatetime
                               creationUser         = ls_data_final-creationuser
                               lastModifiedUser     = ls_data_final-lastmodifieduser
                               %control             =
                               VALUE #(
                                        zuuid                 = if_abap_behv=>mk-on
                                        zbofirstname          = if_abap_behv=>mk-on
                                        createdAt            = if_abap_behv=>mk-off
                                        creationUser         = if_abap_behv=>mk-off
                                        zboemail                = if_abap_behv=>mk-on
                                        zbolastname           = if_abap_behv=>mk-on
                                        lastModifiedDateTime = if_abap_behv=>mk-off
                                        lastModifiedUser     = if_abap_behv=>mk-off )
                               ) ).

            MODIFY ENTITIES OF zr_bo_emailupload IN LOCAL MODE
            ENTITY zrboemailupload
            UPDATE FIELDS ( Zuuid ZboFirstname ZboLastname ZboEmail Createdat Lastmodifieddatetime Creationuser Lastmodifieduser ) WITH lt_data_update
            MAPPED DATA(lt_mapped_u)
            REPORTED DATA(lt_reported_u)
            FAILED DATA(lt_failed_u).

            APPEND LINES OF : lt_reported_u-zrboemailupload TO reported-zrboemailupload ,
                              lt_failed_u-zrboemailupload  TO failed-zrboemailupload ,
                              lt_mapped_u-zrboemailupload  TO mapped-zrboemailupload .

          ENDIF.

        ELSE. "CREATE RECORDS IF FILE CONTAINS ONLY THE RECORDS FOR CREATION

**********insert lt_data_final into lt_data for inserting to DB
          LOOP AT lt_data_final ASSIGNING FIELD-SYMBOL(<ls_data_for_create>).
            APPEND <ls_data_for_create> TO lt_data_create.
          ENDLOOP.

          lt_data = VALUE #( FOR ls_data_for_create IN lt_data_create (
                   zuuid                = ls_data_for_create-zuuid
                   zbofirstname         = ls_data_for_create-zbo_firstname
                   zbolastname          = ls_data_for_create-zbo_lastname
                   zboemail             = ls_data_for_create-zbo_email
                   createdAt            = ls_data_for_create-createdat
                   lastModifiedDateTime = ls_data_for_create-lastmodifieddatetime
                   creationUser         = ls_data_for_create-creationuser
                   lastModifiedUser     = ls_data_for_create-lastmodifieduser
                   %control             =
                   VALUE #(
                            zuuid                 = if_abap_behv=>mk-on
                            zbofirstname          = if_abap_behv=>mk-on
                            createdAt            = if_abap_behv=>mk-off
                            creationUser         = if_abap_behv=>mk-off
                            zboemail                = if_abap_behv=>mk-on
                            zbolastname           = if_abap_behv=>mk-on
                            lastModifiedDateTime = if_abap_behv=>mk-off
                            lastModifiedUser     = if_abap_behv=>mk-off )
                   ) ).

          MODIFY ENTITIES OF zr_bo_emailupload IN LOCAL MODE
          ENTITY zrboemailupload
          CREATE AUTO FILL CID FIELDS ( Zuuid ZboFirstname ZboLastname ZboEmail Createdat Lastmodifieddatetime Creationuser Lastmodifieduser ) WITH lt_data
          MAPPED DATA(lt_mapped)
          REPORTED DATA(lt_reported)
          FAILED DATA(lt_failed).

          APPEND LINES OF : lt_reported-zrboemailupload TO reported-zrboemailupload ,
                            lt_failed-zrboemailupload  TO failed-zrboemailupload ,
                            lt_mapped-zrboemailupload  TO mapped-zrboemailupload .
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
