module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Id
import Type
import Val



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type EditMode
    = EditingSchema
    | EditingValues



-- An invariant here is that the type of the value and the type must match.


type alias Model =
    { val : Val.Val
    , editMode : EditMode
    , idGenerator : Id.Generator
    }


init : Model
init =
    let
        ( nameId, gen1 ) =
            Id.generate Id.init

        ( producerId, gen2 ) =
            Id.generate gen1

        ( modelsId, gen3 ) =
            Id.generate gen2

        ( sizeId, gen4 ) =
            Id.generate gen3

        modelType =
            Type.emptyRecord
                |> Type.addField { name = "Groesse", ty = Type.TyText Type.text, id = sizeId }

        articleRecordType =
            Type.emptyRecord
                |> Type.addField { name = "Name", ty = Type.TyText Type.text, id = nameId }
                |> Type.addField { name = "Hersteller", ty = Type.TyText Type.text, id = producerId }
                |> Type.addField { name = "Modelle", ty = Type.TyTable modelType, id = modelsId }

        ( emptyArticle1, gen5 ) =
            Val.initialRecord gen3 articleRecordType

        ( emptyArticle2, gen6 ) =
            Val.initialRecord gen4 articleRecordType

        dummyText =
            Val.ValText "INIT FAILED"

        emptyArticleTable =
            Val.initialTable articleRecordType

        articlesTable =
            Result.withDefault emptyArticleTable
                (Val.tableInsert
                    (Result.withDefault emptyArticleTable
                        (Val.tableInsert emptyArticleTable emptyArticle1)
                    )
                    emptyArticle2
                )
    in
    { val = Val.ValTable articlesTable, idGenerator = gen6, editMode = EditingValues }



-- UPDATE


type Msg
    = SetField Type.Path Val.Val
    | SetEditMode EditMode
    | SetFieldName Id.Id String
    | AddField
    | AddRecordToTable Type.Record Type.Path


addRecordToTable : Val.Val -> Val.Record -> Result String Val.Val
addRecordToTable val row =
    case val of
        Val.ValText _ ->
            Err "Internal error: Table expected here (got text)"

        Val.ValRecord _ ->
            Err "Internal error: Table expected here (got record)"

        Val.ValTable tbl ->
            Result.map Val.ValTable (Val.tableInsert tbl row)


update : Msg -> Model -> Model
update msg model =
    case Debug.log "update" msg of
        SetEditMode editMode ->
            { model | editMode = editMode }

        SetField path fieldValue ->
            case Val.map model.val path (\_ -> Ok fieldValue) of
                Err errorMsg ->
                    Debug.log errorMsg model

                Ok v ->
                    { model | val = v }

        AddRecordToTable recordType pathToTable ->
            let
                ( row, newGen ) =
                    Val.initialRecord model.idGenerator recordType
            in
            case Val.map model.val pathToTable (\tbl -> addRecordToTable tbl row) of
                Ok v ->
                    { model | val = v, idGenerator = newGen }

                Err errorMsg ->
                    Debug.log errorMsg model

        _ ->
            model



{-
   SetField fieldId fieldValue ->
       { model | record = Record.set model.record fieldId fieldValue }

   SetFieldName fieldId fieldName ->
       { model | record = Record.setFieldName model.record fieldId fieldName }

   AddField ->
       { model | record = Record.addField model.record "" Schema.KindText }

   AddRecord ->
       model
-}
-- VIEW


headerBackgroundColor =
    Background.color (rgb255 0x3C 0x42 0x45)


headerFontColor =
    Font.color (rgb255 255 255 255)


tableBackgroundColor =
    Background.color (rgb255 0x71 0x91 0x92)


activeCelBackgroundColor =
    Background.color (rgb255 0xDF 0xCD 0xC3)



{- viewColumn : Maybe CellNdx -> Int -> Column -> Element.IndexedColumn Row Msg
   viewColumn selection columnNdx { label } =
       let
           header =
               el [ headerBackgroundColor, headerFontColor, paddingXY 4 4 ] (text label)

           viewOrEditCell rowNdx { cells } =
               let
                   content =
                       Maybe.withDefault "" (Array.get columnNdx cells)

                   viewCell () =
                       el
                           [ paddingXY 4 4
                           , Events.onClick (FocusCell { row = rowNdx, column = columnNdx })
                           , tableBackgroundColor
                           ]
                           (text content)
               in
               case selection of
                   Nothing ->
                       viewCell ()

                   Just activeCell ->
                       if activeCell.row == rowNdx && activeCell.column == columnNdx then
                           Input.text [ paddingXY 4 4 ]
                               { onChange = CellChanged activeCell
                               , text = content
                               , placeholder = Nothing
                               , label = Input.labelHidden "cell"
                               }

                       else
                           viewCell ()
       in
       { header = header, width = minimum 50 shrink, view = viewOrEditCell }


   viewTable : Table -> Element Msg
   viewTable table =
       let
           columns =
               List.indexedMap (viewColumn table.activeCell) table.columns

           data =
               table.rows

           tableAttributes =
               [ tableBackgroundColor, centerX, width shrink ]
       in
       indexedTable tableAttributes { columns = columns, data = data }
-}
{-
   viewEditList : List Record.Record -> Element Msg
   viewEditList l =
       column [ spacing 5 ]
           (List.map viewEditRecord l
               ++ [ Input.button
                       [ Background.color (rgb255 0 0 230)
                       , Element.focused [ Background.color (rgb255 0 0 230) ]
                       , Font.color (rgb255 255 255 255)
                       ]
                       { onPress = Just AddRecord
                       , label = text "+"
                       }
                  ]
           )


   viewFieldValue : Schema.Field -> Record.Val -> Element Msg
   viewFieldValue field val =
       case val of
           Record.ValList l ->
               viewEditList l

           Record.ValText t ->
               Input.text []
                   { onChange = \txt -> SetField (Schema.fieldId field) (Record.ValText txt)
                   , text = t
                   , placeholder = Nothing
                   , label = Input.labelHidden "A field"
                   }

           Record.ValRecord _ ->
               text "A record"


   viewEditRecord : Record.Record -> Element Msg
   viewEditRecord record =
       table
           [ Border.color (rgb255 0 0 0)
           , Border.rounded 5
           , Border.width 1
           , padding 10
           , width shrink
           , height shrink
           , spacing 5
           ]
           { columns =
               [ Column none
                   (minimum 50 shrink)
                   (\( field, _ ) ->
                       el [ Font.bold, centerY ] (text (Schema.fieldName field))
                   )
               , Column none (minimum 80 shrink) (\( field, val ) -> viewFieldValue field val)
               ]
           , data = Record.toList record
           }


   viewDesignField : Schema.Field -> Element Msg
   viewDesignField field =
       let
           kind =
               Schema.fieldKind field

           kindText =
               case kind of
                   Schema.KindText ->
                       "Text"

                   Schema.KindList _ ->
                       "List"
       in
       el [ centerY ] (text kindText)


   viewDesignRecord : Record.Record -> Element Msg
   viewDesignRecord record =
       column
           [ Border.color (rgb255 0 0 0)
           , Border.rounded 5
           , Border.width 1
           , padding 10
           , width shrink
           , height shrink
           , spacing 5
           ]
           [ table
               [ width shrink
               , height shrink
               , spacing 5
               ]
               { columns =
                   [ Column none
                       (minimum 50 shrink)
                       (\( field, _ ) ->
                           Input.text [ centerY ]
                               { onChange = SetFieldName (Schema.fieldId field)
                               , text = Schema.fieldName field
                               , placeholder = Nothing
                               , label = Input.labelHidden "fieldname"
                               }
                       )
                   , Column none (minimum 80 shrink) (\( field, _ ) -> viewDesignField field)
                   ]
               , data = Record.toList record
               }
           , Input.button
               [ Background.color (rgb255 0 0 230)
               , Element.focused [ Background.color (rgb255 0 0 230) ]
               , Font.color (rgb255 255 255 255)
               ]
               { onPress = Just AddField
               , label = text "+"
               }
           ]
-}


viewRecordFieldValue : Type.Path -> Val.Val -> Element Msg
viewRecordFieldValue path val =
    viewEditor path val


viewRecordEditor : Type.Path -> Val.Record -> Element Msg
viewRecordEditor path record =
    table
        [ Border.color (rgb255 0 0 0)
        , Border.rounded 5
        , Border.width 1
        , padding 10
        , width shrink
        , height shrink
        , spacing 5
        ]
        { columns =
            [ Column none
                (minimum 50 shrink)
                (\( field, _ ) ->
                    el [ Font.bold, centerY ] (text (Type.fieldName field))
                )
            , Column none
                (minimum 80 shrink)
                (\( field, v ) ->
                    viewRecordFieldValue (path ++ [ Type.fieldId field ]) v
                )
            ]
        , data = Val.recordToList record
        }


viewEditor : Type.Path -> Val.Val -> Element Msg
viewEditor path val =
    case val of
        Val.ValText txt ->
            Input.text []
                { onChange = \t -> SetField path (Val.ValText t)
                , text = txt
                , placeholder = Nothing
                , label = Input.labelHidden "A field"
                }

        Val.ValTable table ->
            column [ padding 5, spacing 5, width fill, height fill, scrollbarY ]
                (List.map
                    (\record ->
                        --row [ spacing 5, width fill ]
                        viewRecordEditor (path ++ [ Val.recordId record ]) record
                     --   , Input.button
                     --       -- 3B 99 FC
                     --       [ Background.color (rgb255 0x3B 0x99 0xFC)
                     --       , Font.color (rgb255 255 255 255)
                     --       , Font.bold
                     --       , Border.rounded 3
                     --       , padding 3
                     --       ]
                     --       { onPress = Nothing -- Just (RemoveRowFromTable [])
                     --       , label = text "X"
                     --       }
                     --   ]
                    )
                    (Val.tableToRecords table)
                    ++ [ Input.button
                            -- 3B 99 FC
                            [ Background.color (rgb255 0x3B 0x99 0xFC)
                            , Font.color (rgb255 255 255 255)
                            , Font.bold
                            , Border.rounded 3
                            , padding 3
                            ]
                            { onPress = Just (AddRecordToTable (Val.tableRecordType table) path)
                            , label = text "+"
                            }
                       ]
                )

        Val.ValRecord record ->
            viewRecordEditor path record


viewDesigner : Val.Val -> Element Msg
viewDesigner val =
    text "TODO"


view : Model -> Html Msg
view { val, editMode } =
    let
        header =
            row [ Background.color (rgb255 100 100 100), width fill, height shrink ]
                [ Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = SetEditMode
                    , selected = Just editMode
                    , label = Input.labelHidden "Mode"
                    , options =
                        [ Input.option EditingValues (text "Edit")
                        , Input.option EditingSchema (text "Design")
                        ]
                    }
                ]
    in
    layout [ width fill, height fill ] <|
        column
            [ spacing 10, width fill, height fill ]
            [ header
            , case editMode of
                EditingSchema ->
                    viewDesigner val

                EditingValues ->
                    viewEditor [] val
            ]
