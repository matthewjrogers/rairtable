#' Airtable field types
#'
#' A vector of Airtable field types as of 2023-04-18. Find more information on
#' Airtable field types at
#' <https://airtable.com/developers/web/api/model/field-type>
#'
# field_types <-
#   c(
#     "singleLineText", "email", "url", "multilineText", "number", "percent",
#     "currency", "singleSelect", "multipleSelects", "singleCollaborator",
#     "multipleCollaborators", "multipleRecordLinks", "date", "dateTime",
#     "phoneNumber", "multipleAttachments", "checkbox", "formula", "createdTime",
#     "rollup", "count", "lookup", "multipleLookupValues", "autoNumber",
#     "barcode", "rating", "richText", "duration", "lastModifiedTime", "button",
#     "createdBy", "lastModifiedBy", "externalSyncSource"
#   )

field_types <-
  tibble::tribble(
                      ~type,                    ~name,                                                                             ~url,      ~class, ~read_only, ~options,
               "autoNumber",            "Auto number",               "https://airtable.com//developers/web/api/field-model#auto-number", "character",       TRUE,     TRUE,
                  "barcode",                "Barcode",                   "https://airtable.com//developers/web/api/field-model#barcode", "character",       TRUE,     TRUE,
                   "button",                 "Button",                    "https://airtable.com//developers/web/api/field-model#button", "character",       TRUE,     TRUE,
                 "checkbox",               "Checkbox",                  "https://airtable.com//developers/web/api/field-model#checkbox",   "boolean",      FALSE,     TRUE,
                    "count",                  "Count",                     "https://airtable.com//developers/web/api/field-model#count", "character",       TRUE,     TRUE,
                "createdBy",             "Created by",                "https://airtable.com//developers/web/api/field-model#created-by", "character",       TRUE,     TRUE,
              "createdTime",           "Created time",              "https://airtable.com//developers/web/api/field-model#created-time", "character",       TRUE,     TRUE,
                 "currency",               "Currency",           "https://airtable.com//developers/web/api/field-model#currency-number",   "numeric",      FALSE,     TRUE,
                     "date",                   "Date",                 "https://airtable.com//developers/web/api/field-model#date-only",      "date",      FALSE,     TRUE,
                 "dateTime",          "Date and time",             "https://airtable.com//developers/web/api/field-model#date-and-time",      "date",      FALSE,     TRUE,
                 "duration",               "Duration",           "https://airtable.com//developers/web/api/field-model#duration-number", "character",      FALSE,     TRUE,
                    "email",                  "Email",                "https://airtable.com//developers/web/api/field-model#email-text", "character",      FALSE,    FALSE,
       "externalSyncSource",            "Sync source",               "https://airtable.com//developers/web/api/field-model#sync-source", "character",      FALSE,     TRUE,
                  "formula",                "Formula",                   "https://airtable.com//developers/web/api/field-model#formula", "character",       TRUE,     TRUE,
           "lastModifiedBy",       "Last modified by",          "https://airtable.com//developers/web/api/field-model#last-modified-by", "character",       TRUE,    FALSE,
         "lastModifiedTime",     "Last modified time",        "https://airtable.com//developers/web/api/field-model#last-modified-time", "character",       TRUE,     TRUE,
                   "lookup",                 "Lookup",                    "https://airtable.com//developers/web/api/field-model#lookup", "character",       TRUE,     TRUE,
            "multilineText",              "Long text",            "https://airtable.com//developers/web/api/field-model#multiline-text", "character",      FALSE,    FALSE,
      "multipleAttachments",             "Attachment",       "https://airtable.com//developers/web/api/field-model#multiple-attachment", "character",      FALSE,     TRUE,
    "multipleCollaborators",  "Multiple collaborator",        "https://airtable.com//developers/web/api/field-model#multi-collaborator", "character",      FALSE,     TRUE,
     "multipleLookupValues",                 "Lookup",                    "https://airtable.com//developers/web/api/field-model#lookup", "character",      FALSE,     TRUE,
      "multipleRecordLinks", "Link to another record",               "https://airtable.com//developers/web/api/field-model#foreign-key", "character",      FALSE,     TRUE,
          "multipleSelects",        "Multiple select",              "https://airtable.com//developers/web/api/field-model#multi-select", "character",      FALSE,     TRUE,
                   "number",                 "Number", "https://airtable.com//developers/web/api/field-model#decimal-or-integer-number",   "numeric",      FALSE,     TRUE,
                  "percent",                "Percent",            "https://airtable.com//developers/web/api/field-model#percent-number",   "numeric",      FALSE,     TRUE,
              "phoneNumber",                  "Phone",                     "https://airtable.com//developers/web/api/field-model#phone", "character",      FALSE,    FALSE,
                   "rating",                 "Rating",                    "https://airtable.com//developers/web/api/field-model#rating", "character",      FALSE,     TRUE,
                 "richText",              "Rich text",                 "https://airtable.com//developers/web/api/field-model#rich-text", "character",      FALSE,     TRUE,
                   "rollup",                 "Rollup",                    "https://airtable.com//developers/web/api/field-model#rollup", "character",       TRUE,     TRUE,
       "singleCollaborator",           "Collaborator",              "https://airtable.com//developers/web/api/field-model#collaborator", "character",      FALSE,     TRUE,
           "singleLineText",       "Single line text",               "https://airtable.com//developers/web/api/field-model#simple-text", "character",      FALSE,    FALSE,
             "singleSelect",          "Single select",                    "https://airtable.com//developers/web/api/field-model#select", "character",      FALSE,     TRUE,
                      "url",                    "Url",                  "https://airtable.com//developers/web/api/field-model#url-text", "character",      FALSE,    FALSE
    )

usethis::use_data(field_types, overwrite = TRUE)
