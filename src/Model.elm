module Model exposing (Model, StatueSelection, ensureNoIllegalSelections, ensureUniqueInsideShapes, initModel, selected2Dshapes, selected3Dshapes, solveShapes)

import Shapes exposing (Shape2D(..), Shape3D, isIllegalShapeToStart)
import Statues.Internal exposing (Position(..))
import Steps.Internal exposing (Step)


type alias StatueSelection =
    { insideShape : Maybe Shape2D
    , outsideShape : Maybe Shape3D
    }


type alias Model =
    { leftStatueSelections : StatueSelection
    , middleStatueSelections : StatueSelection
    , rightStatueSelections : StatueSelection
    , steps : List Step
    }


emptySelection : StatueSelection
emptySelection =
    { insideShape = Nothing, outsideShape = Nothing }


initModel : Model
initModel =
    { leftStatueSelections = emptySelection, middleStatueSelections = emptySelection, rightStatueSelections = emptySelection, steps = [] }


selected2Dshapes : Model -> List Shape2D
selected2Dshapes model =
    selectedShapes [ model.leftStatueSelections.insideShape, model.middleStatueSelections.insideShape, model.rightStatueSelections.insideShape ]


selected3Dshapes : Model -> List Shape3D
selected3Dshapes model =
    selectedShapes [ model.leftStatueSelections.outsideShape, model.middleStatueSelections.outsideShape, model.rightStatueSelections.outsideShape ]


selectedShapes : List (Maybe s) -> List s
selectedShapes shapes =
    List.filterMap identity shapes


solveShapes : Model -> Model
solveShapes model =
    model


ensureNoIllegalSelections : Model -> Model
ensureNoIllegalSelections model =
    let
        leftSelections =
            model.leftStatueSelections

        middleSelections =
            model.middleStatueSelections

        rightSelections =
            model.rightStatueSelections
    in
    { model
        | leftStatueSelections =
            if
                Maybe.withDefault False <|
                    Maybe.map2 isIllegalShapeToStart leftSelections.insideShape leftSelections.outsideShape
            then
                { leftSelections | outsideShape = Nothing }

            else
                leftSelections
        , middleStatueSelections =
            if
                Maybe.withDefault False <|
                    Maybe.map2 isIllegalShapeToStart middleSelections.insideShape middleSelections.outsideShape
            then
                { middleSelections | outsideShape = Nothing }

            else
                middleSelections
        , rightStatueSelections =
            if
                Maybe.withDefault False <|
                    Maybe.map2 isIllegalShapeToStart rightSelections.insideShape rightSelections.outsideShape
            then
                { rightSelections | outsideShape = Nothing }

            else
                rightSelections
    }


ensureUniqueInsideShapes : Position -> Model -> Model
ensureUniqueInsideShapes posToIgnore model =
    let
        selections =
            List.filterMap identity [ model.leftStatueSelections.insideShape, model.middleStatueSelections.insideShape, model.rightStatueSelections.insideShape ]
    in
    { model
        | leftStatueSelections =
            if posToIgnore == Left then
                model.leftStatueSelections

            else
                ensureUniqueShapes2D selections model.leftStatueSelections
        , middleStatueSelections =
            if posToIgnore == Middle then
                model.middleStatueSelections

            else
                ensureUniqueShapes2D selections model.middleStatueSelections
        , rightStatueSelections =
            if posToIgnore == Right then
                model.rightStatueSelections

            else
                ensureUniqueShapes2D selections model.rightStatueSelections
    }


ensureUniqueShapes2D : List Shape2D -> StatueSelection -> StatueSelection
ensureUniqueShapes2D shapes shapesSelected =
    case shapesSelected.insideShape of
        Just s ->
            if List.length (List.filter ((==) s) shapes) > 1 then
                emptySelection

            else
                shapesSelected

        Nothing ->
            shapesSelected
