module Schema exposing (Field, FieldId, FieldKind(..), Path, Schema, addField, empty, fieldId, fieldKind, fieldName, setFieldName, toList)

import Array
import Dict


type alias FieldId =
    Int


type Path
    = Path (List FieldId)


type alias Field =
    { name : String
    , kind : FieldKind
    , id : FieldId
    }


type alias Schema =
    { fields : List Field
    , nextFreeId : Int
    }


type FieldKind
    = KindList Schema
    | KindText


empty : Schema
empty =
    { fields = [], nextFreeId = 0 }


addField : Schema -> { name : String, kind : FieldKind } -> ( FieldId, Schema )
addField { fields, nextFreeId } { name, kind } =
    let
        id =
            nextFreeId

        field =
            { name = name, kind = kind, id = id }
    in
    ( id, { fields = fields ++ [ field ], nextFreeId = nextFreeId + 1 } )


setFieldName : Schema -> FieldId -> String -> Schema
setFieldName schema id name =
    { schema
        | fields =
            List.map
                (\field ->
                    if field.id == id then
                        { field | name = name }

                    else
                        field
                )
                schema.fields
    }


fieldName : Field -> String
fieldName { name } =
    name


fieldKind : Field -> FieldKind
fieldKind { kind } =
    kind


fieldId : Field -> Int
fieldId { id } =
    id


toList : Schema -> List Field
toList { fields } =
    fields
