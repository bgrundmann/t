module Type exposing (Field, Path, Record, Text, Type(..), addField, emptyRecord, fieldId, fieldName, fieldType, recordFields, text)

{-| This library represents the type / schema of the data stored.
-}

import Array
import Dict
import Id


type Type
    = TyText Text
    | TyTable Record
    | TyRecord Record


type alias Path =
    List Id.Id


type Text
    = Text {}


{-| A unconstrained text
-}
text : Text
text =
    Text {}


type Record
    = Record
        { fields : List Field
        }


{-| A empty record (that is a record with no fields).
-}
emptyRecord : Record
emptyRecord =
    Record { fields = [] }


{-| The list of fields in a record. In the order they are displayed.
-}
recordFields : Record -> List Field
recordFields (Record { fields }) =
    fields


{-| Add a field to a record.
-}
addField : { name : String, ty : Type, id : Id.Id } -> Record -> Record
addField { name, ty, id } (Record r) =
    Record { r | fields = r.fields ++ [ Field { name = name, ty = ty, id = id } ] }


{-| Change the name of a field.
-}
setFieldName : Record -> Id.Id -> String -> Record
setFieldName (Record r) id name =
    Record
        { r
            | fields =
                List.map
                    (\(Field field) ->
                        if field.id == id then
                            Field { field | name = name }

                        else
                            Field field
                    )
                    r.fields
        }


type Field
    = Field
        { name : String
        , ty : Type
        , id : Id.Id
        }


fieldName : Field -> String
fieldName (Field { name }) =
    name


fieldType : Field -> Type
fieldType (Field { ty }) =
    ty


fieldId : Field -> Id.Id
fieldId (Field { id }) =
    id
