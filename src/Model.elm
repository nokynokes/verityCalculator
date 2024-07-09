module Model exposing (Model, StatueSelection, ensureLimit2DShapes, ensureNoIllegalSelections, ensureUniqueInsideShapes, initModel, maxNumberOf2DShapes, numberOfCircles, numberOfSquares, numberOfTriangles, selected2Dshapes, selected3Dshapes, solveShapes)

import Calculator exposing (orderToSolve)
import Shapes exposing (Shape2D(..), Shape3D(..), isIllegalShapeToStart)
import Statues exposing (Statue)
import Statues.Internal exposing (Position(..))
import Steps exposing (generateSteps)
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


maxNumberOf2DShapes : Int
maxNumberOf2DShapes =
    2


numberOfShapes : Shape2D -> Shape3D -> Int
numberOfShapes s1 s2 =
    case ( s1, s2 ) of
        ( Circle, Sphere ) ->
            2

        ( Circle, Cone ) ->
            1

        ( Circle, Cylinder ) ->
            1

        ( Square, Cube ) ->
            2

        ( Square, Prism ) ->
            1

        ( Square, Cylinder ) ->
            1

        ( Triangle, Pyramid ) ->
            2

        ( Triangle, Cone ) ->
            1

        ( Triangle, Prism ) ->
            1

        _ ->
            0


numberOfCircles : List Shape3D -> Int
numberOfCircles =
    numberOf2DShapesSelected Circle


numberOfSquares : List Shape3D -> Int
numberOfSquares =
    numberOf2DShapesSelected Square


numberOfTriangles : List Shape3D -> Int
numberOfTriangles =
    numberOf2DShapesSelected Triangle


numberOf2DShapesSelected : Shape2D -> List Shape3D -> Int
numberOf2DShapesSelected shape shapes3D =
    List.foldl
        (\s acc -> acc + numberOfShapes shape s)
        0
        shapes3D


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


selectionToStatue : ( Position, StatueSelection ) -> Statue
selectionToStatue ( pos, selection ) =
    { position = pos
    , insideShape = Maybe.withDefault Circle selection.insideShape
    , outsideShape = Maybe.withDefault Sphere selection.outsideShape
    }


solveShapes : Model -> Model
solveShapes model =
    let
        insideSelections =
            (selected2Dshapes >> List.length) model

        outsideSelections =
            (selected3Dshapes >> List.length) model
    in
    if insideSelections /= 3 || outsideSelections /= 3 then
        { model | steps = [] }

    else
        let
            statues =
                List.map selectionToStatue <|
                    List.map2 Tuple.pair
                        [ Left, Middle, Right ]
                        [ model.leftStatueSelections, model.middleStatueSelections, model.rightStatueSelections ]
        in
        { model
            | steps = (orderToSolve >> generateSteps) statues
        }


ensureNoIllegalSelections : Model -> Model
ensureNoIllegalSelections model =
    { model
        | leftStatueSelections =
            Maybe.map2 isIllegalShapeToStart model.leftStatueSelections.insideShape model.leftStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.leftStatueSelections
        , middleStatueSelections =
            Maybe.map2 isIllegalShapeToStart model.middleStatueSelections.insideShape model.middleStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.middleStatueSelections
        , rightStatueSelections =
            Maybe.map2 isIllegalShapeToStart model.rightStatueSelections.insideShape model.rightStatueSelections.outsideShape
                |> Maybe.withDefault False
                |> ensureOutsideSelection model.rightStatueSelections
    }


ensureLimit2DShapes : Position -> Model -> Model
ensureLimit2DShapes posToIgnore model =
    let
        selections =
            List.filterMap identity [ model.leftStatueSelections.outsideShape, model.middleStatueSelections.outsideShape, model.rightStatueSelections.outsideShape ]

        verifySelections =
            ensureOrIgnoreShape (ensureLimts selections) posToIgnore
    in
    { model
        | leftStatueSelections =
            verifySelections Left model.leftStatueSelections
        , middleStatueSelections =
            verifySelections Middle model.middleStatueSelections
        , rightStatueSelections =
            verifySelections Right model.rightStatueSelections
    }


ensureLimts : List Shape3D -> StatueSelection -> StatueSelection
ensureLimts shapes selection =
    let
        condition =
            case selection.outsideShape of
                Just Sphere ->
                    numberOfCircles shapes > maxNumberOf2DShapes

                Just Cube ->
                    numberOfSquares shapes > maxNumberOf2DShapes

                Just Pyramid ->
                    numberOfTriangles shapes > maxNumberOf2DShapes

                Just Cone ->
                    numberOfTriangles shapes > maxNumberOf2DShapes || numberOfCircles shapes > maxNumberOf2DShapes

                Just Cylinder ->
                    numberOfCircles shapes > maxNumberOf2DShapes || numberOfSquares shapes > maxNumberOf2DShapes

                Just Prism ->
                    numberOfTriangles shapes > maxNumberOf2DShapes || numberOfSquares shapes > maxNumberOf2DShapes

                Nothing ->
                    False
    in
    ensureOutsideSelection selection condition


ensureOutsideSelection : StatueSelection -> Bool -> StatueSelection
ensureOutsideSelection selection condition =
    if condition then
        { selection | outsideShape = Nothing }

    else
        selection


ensureOrIgnoreShape : (StatueSelection -> StatueSelection) -> Position -> Position -> StatueSelection -> StatueSelection
ensureOrIgnoreShape transform ignorePos pos selection =
    if pos == ignorePos then
        selection

    else
        transform selection


ensureUniqueInsideShapes : Position -> Model -> Model
ensureUniqueInsideShapes posToIgnore model =
    let
        selections =
            selected2Dshapes model

        verifySelections =
            ensureOrIgnoreShape (ensureUniqueShapes2D selections) posToIgnore
    in
    { model
        | leftStatueSelections =
            verifySelections Left model.leftStatueSelections
        , middleStatueSelections =
            verifySelections Middle model.middleStatueSelections
        , rightStatueSelections =
            verifySelections Right model.rightStatueSelections
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
