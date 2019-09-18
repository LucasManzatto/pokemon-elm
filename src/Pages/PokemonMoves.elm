module Pages.PokemonMoves exposing (view)

import Browser exposing (UrlRequest(..))
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as List
import Pages.Shared exposing (..)
import Platform.Cmd exposing (Cmd)
import Types exposing (..)
import Utils exposing (..)


view : List PokemonMoves -> Html msg
view pokemonMoves =
    let
        groupedMoves =
            List.gatherEqualsBy .learnMethods pokemonMoves

        groupedMovesOrderList =
            [ "level-up", "egg", "machine", "tutor" ]

        orderedGroupMoves =
            groupedMovesOrderList
                |> List.map
                    (\learnMethod ->
                        case List.find (\( firstMove, _ ) -> firstMove.learnMethods == learnMethod) groupedMoves of
                            Just ( firstMove, rest ) ->
                                let
                                    ( headerText, firstHeaderColumn, sortedMovesRow ) =
                                        case learnMethod of
                                            "level-up" ->
                                                ( "Leveling"
                                                , th [ scope "col", class "text-right" ]
                                                    [ text "Level" ]
                                                , List.sortBy .name (firstMove :: rest)
                                                )

                                            "machine" ->
                                                ( "TM"
                                                , th [ scope "col", class "text-right" ]
                                                    [ text "TM" ]
                                                , firstMove :: rest
                                                )

                                            "egg" ->
                                                ( "Egg", text "", firstMove :: rest )

                                            "tutor" ->
                                                ( "Tutor", text "", firstMove :: rest )

                                            _ ->
                                                ( "Undefined Group", text "", firstMove :: rest )
                                in
                                div []
                                    [ h2 []
                                        [ text
                                            headerText
                                        ]
                                    , table [ class "table table-hover table-sm text-center" ]
                                        [ thead []
                                            [ tr []
                                                [ firstHeaderColumn
                                                , th [ scope "col", class "text-left" ]
                                                    [ text "Move" ]
                                                , th [ scope "col" ]
                                                    [ text "Type" ]
                                                , th [ scope "col" ]
                                                    [ text "Category" ]
                                                , th [ scope "col" ]
                                                    [ text "Power" ]
                                                , th [ scope "col" ]
                                                    [ text "Accuracy" ]
                                                ]
                                            ]
                                        , tbody [ class "font-weight-normal" ]
                                            (List.map
                                                (\moveRow ->
                                                    let
                                                        firstRowColumn =
                                                            if learnMethod == "level-up" then
                                                                case moveRow.level of
                                                                    Just level ->
                                                                        th
                                                                            [ class "text-right"
                                                                            , style "vertical-align" "middle"
                                                                            ]
                                                                            [ text (String.fromInt level) ]

                                                                    Nothing ->
                                                                        th [] [ text "-" ]

                                                            else if learnMethod == "machine" then
                                                                case moveRow.tmMachineNumber of
                                                                    Just tmMachineNumber ->
                                                                        th [ class "text-right" ] [ text (String.fromInt tmMachineNumber) ]

                                                                    Nothing ->
                                                                        th [] [ text "-" ]

                                                            else
                                                                text ""

                                                        formattedName =
                                                            moveRow.name
                                                                |> String.replace "-" " "
                                                                |> String.split " "
                                                                |> List.map capitalize
                                                                |> String.join " "
                                                    in
                                                    tr []
                                                        [ firstRowColumn
                                                        , td [ class "text-left" ]
                                                            [ text
                                                                formattedName
                                                            ]
                                                        , td []
                                                            [ case moveRow.moveType of
                                                                Just moveType ->
                                                                    pokemonTypeFull moveType

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        , td []
                                                            [ img
                                                                [ src ("https://img.pokemondb.net/images/icons/" ++ moveRow.damageClass ++ ".png")
                                                                , style "margin" "0"
                                                                , style "padding" "0"
                                                                ]
                                                                []
                                                            ]
                                                        , td []
                                                            [ case moveRow.power of
                                                                Just power ->
                                                                    text (String.fromInt power)

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        , td []
                                                            [ case moveRow.accuracy of
                                                                Just accuracy ->
                                                                    text (String.fromInt accuracy)

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        ]
                                                )
                                                sortedMovesRow
                                            )
                                        ]
                                    ]

                            Nothing ->
                                div [] []
                    )
                |> List.groupsOf 2

        html =
            div [ class "row" ]
                (List.map
                    (\col ->
                        div [ class "col-6" ]
                            [ div [ class "row" ]
                                [ div [ class "col-12" ]
                                    col
                                ]
                            ]
                    )
                    orderedGroupMoves
                )
    in
    html
