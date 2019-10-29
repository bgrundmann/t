module Val exposing (Record, Table, Val(..), initial, initialRecord, initialTable, map, recordId, recordToList, tableInsert, tableRecordType, tableToRecords)

import Dict
import Id
import Result.Extra as Result
import Type


{-| The type of a value.
-}
type Val
    = ValText String
    | ValTable Table -- All Records in a table must have the same schema
    | ValRecord Record


{-| The type of a table of records (where each record has the same type).
-}
type Table
    = Table Type.Record (Dict.Dict Id.Id Row)


{-| The type of a singular record.
-}
type Record
    = Record Type.Record Row


{-| The type of a row of fields. Used by both record and Table.
-}
type Row
    = Row { fields : List Val, id : Id.Id }



-- A Note on the implementation of a row.  It's a list on the theory
-- that we more often need access to all of the fields in order and
-- that the lists will tend to be small, in which case the simplicity
-- means that we will win.
-- Furthermore the list of fields MUST be in the same order as the
-- fields in Type.


rowId (Row { id }) =
    id


rowFields (Row { fields }) =
    fields


{-| The type of a value.
-}
typeOf : Val -> Type.Type
typeOf v =
    case v of
        ValText _ ->
            Type.TyText Type.text

        ValTable (Table r _) ->
            Type.TyTable r

        ValRecord (Record r _) ->
            Type.TyRecord r


{-| Produce a suitable initial value for a record of the given type.
-}
initialRecord : Id.Generator -> Type.Record -> ( Record, Id.Generator )
initialRecord gen recordType =
    let
        ( id, nextGen ) =
            Id.generate gen

        ( fields, finalGen ) =
            List.foldl
                (\field ( acc, genAcc ) ->
                    let
                        ( value, nextGenAcc ) =
                            initial genAcc (Type.fieldType field)
                    in
                    ( value :: acc, nextGenAcc )
                )
                ( [], nextGen )
                (Type.recordFields recordType)

        row =
            Row { fields = List.reverse fields, id = id }
    in
    ( Record recordType row, finalGen )


{-| Create an empty table.
-}
initialTable : Type.Record -> Table
initialTable tr =
    Table tr Dict.empty


{-| Add a record to a table (or replace if a record of that ID already exist). Fails if the record is of the wrong type.
-}
tableInsert : Table -> Record -> Result String Table
tableInsert (Table tableType rows) (Record recordType row) =
    -- TODO: Checks
    Ok (Table tableType (Dict.insert (rowId row) row rows))


{-| Produce a suitable initital value for the given type.
-}
initial : Id.Generator -> Type.Type -> ( Val, Id.Generator )
initial gen ty =
    case ty of
        Type.TyText _ ->
            ( ValText "", gen )

        Type.TyTable r ->
            ( ValTable (initialTable r), gen )

        Type.TyRecord r ->
            let
                ( record, finalGen ) =
                    initialRecord gen r
            in
            ( ValRecord record, finalGen )


recordRow (Record _ row) =
    row


recordId : Record -> Id.Id
recordId r =
    rowId (recordRow r)


recordToList : Record -> List ( Type.Field, Val )
recordToList (Record tyRecord row) =
    List.map2
        (\field fieldValue -> ( field, fieldValue ))
        (Type.recordFields tyRecord)
        (rowFields row)


tableRecordType : Table -> Type.Record
tableRecordType (Table t _) =
    t


tableToRecords : Table -> List Record
tableToRecords (Table ty rows) =
    List.map (Record ty) (Dict.values rows)


rowMapField : Type.Record -> Row -> Id.Id -> (Val -> Result String Val) -> Result String Row
rowMapField ty row fieldId f =
    List.map2
        (\field fieldValue ->
            if Type.fieldId field == fieldId then
                f fieldValue

            else
                Ok fieldValue
        )
        (Type.recordFields ty)
        (rowFields row)
        |> Result.combine
        |> Result.map (\fields -> Row { fields = fields, id = rowId row })


recordMapField : Record -> Id.Id -> (Val -> Result String Val) -> Result String Record
recordMapField (Record ty row) fieldId f =
    Result.map (Record ty) (rowMapField ty row fieldId f)


{-| Apply f to the given record of a table (if any).
-}
tableMapRecordIfAny : Table -> Id.Id -> (Maybe Record -> Result String Record) -> Result String Table
tableMapRecordIfAny (Table recordTy rows) id f =
    let
        existingRow =
            Dict.get id rows

        existingRowAsRecord =
            Maybe.map (Record recordTy) existingRow

        newRecordOrError =
            f existingRowAsRecord
    in
    Result.map
        (\(Record t newRow) ->
            -- TODO: Sanity Check that the types match
            Table recordTy (Dict.insert id newRow rows)
        )
        newRecordOrError


{-| Apply f to the field at the given path. Note that this function
cannot be used to create new records in a table.
-}
map : Val -> Type.Path -> (Val -> Result String Val) -> Result String Val
map root path f =
    case path of
        [] ->
            f root

        p :: pathRest ->
            case root of
                ValText _ ->
                    Err "Val.set: Text values do not have children."

                ValTable table ->
                    tableMapRecordIfAny table
                        p
                        (\existingRecord ->
                            case existingRecord of
                                Nothing ->
                                    Err "Val.map: cannot create new rows."

                                Just r ->
                                    case map (ValRecord r) pathRest f of
                                        Err msg ->
                                            Err msg

                                        Ok (ValRecord newRecord) ->
                                            Ok newRecord

                                        Ok _ ->
                                            Err "Cannot change the rows of a table into values that are not records"
                        )
                        |> Result.map ValTable

                ValRecord record ->
                    Result.map
                        ValRecord
                        (recordMapField record p (\field -> map field pathRest f))



{- case path of
   [] ->
       Ok val

   [ fieldOrRowId ] ->
       case root of
           ValText _ ->
               Err "Cannot set a child value inside a text"

           ValRecord (Record tr (Row { fields, id })) ->
               -- TODO: Check that path is valid aka that id is a valid element in row
               Ok (ValRecord (Record tr (Row { fields = Dict.insert fieldOrRowId val fields, id = id })))

           ValTable table ->
               case val of
                   ValRecord record ->
                       if recordId record == fieldOrRowId then
                           Result.map ValTable
                               (tableInsert table record)

                       else
                           Err "Internal error: ID mismatch"

                   ValTable _ ->
                       Err "Mismatch expected record got table"

                   ValText _ ->
                       Err "Mismatch expected record got text"

   _ ->
       Err "Not yet implemented"
-}
