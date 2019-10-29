module Record exposing (Record, Val(..), addField, create, set, setFieldName, toList)

import Dict
import Schema


type Val
    = ValList (List Record)
    | ValText String
    | ValRecord Record


type alias Record =
    { schema : Schema.Schema
    , fields : Dict.Dict Schema.FieldId Val
    }


initialValue fieldKind =
    case fieldKind of
        Schema.KindText ->
            ValText ""

        Schema.KindList schema ->
            ValList []


create : Schema.Schema -> Record
create schema =
    { schema = schema
    , fields = Dict.empty
    }


set : Record -> Schema.FieldId -> Val -> Record
set { schema, fields } id val =
    { schema = schema, fields = Dict.insert id val fields }



{- setPath : Record -> Schema.Path -> Val -> Error String Record
   setPath record path val =
     case path of
       []    -> Err "Invalid path"
       [id] -> Ok (set record path val)
       id :: ids ->
           case setPath
-}


setFieldName : Record -> Schema.FieldId -> String -> Record
setFieldName record id fieldName =
    { record | schema = Schema.setFieldName record.schema id fieldName }


addField : Record -> String -> Schema.FieldKind -> Record
addField record name kind =
    let
        ( id, schema ) =
            Schema.addField record.schema { name = name, kind = kind }
    in
    { record | schema = schema }


toList : Record -> List ( Schema.Field, Val )
toList { schema, fields } =
    List.map
        (\field ->
            let
                value =
                    case Dict.get (Schema.fieldId field) fields of
                        Nothing ->
                            initialValue (Schema.fieldKind field)

                        Just v ->
                            v
            in
            ( field, value )
        )
        (Schema.toList schema)
