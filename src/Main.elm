module Main exposing (main, update, view)

import Browser
import Browser.Events exposing (onKeyPress)
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { viewCenterX : Float
    , viewCenterY : Float
    , viewWidthFeet : Float
    , viewHeightFeet : Float
    , viewWidthPixels : Int
    , viewHeightPixels : Int
    , roadCenterline : RoadCenterline
    , cars : List Car
    }


type alias Car =
    { centerX : Float
    , centerY : Float
    , courseDeg : Float
    , widthFeet : Float
    , lengthFeet : Float
    }


type alias RoadCenterline =
    { originXY : ( Float, Float )
    , originCourseDeg : Float
    , segments : List Segment -- List (Float, Float)
    }


type Segment
    = Linear Float Float
    | Arc Float Float


type Msg
    = TimeUpdate Time.Posix
    | CharacterKey Char
    | ControlKey String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 100.0 100.0 200.0 200.0 800 800 (RoadCenterline ( 100.0, 100.0 ) 0.0 [ Linear 50.0 0.0, Arc 50.0 30.0, Linear 40.0 30.0, Arc 50.0 -30.0, Linear 50.0 0.0 ]) initCars, Cmd.none )


initCars =
    [ Car 100.0 100.0 0.0 10.0 20.0 ]



--Arc 50.0 -90.0 ]), Cmd.none )
--[ ( 100.0, 0.0 ), ( 100.0, 1000.0 ), ( 100.0, 2000.0 ) ], Cmd.none )


updateCars cars angleChangeDeg =
    List.map (\c -> { c | courseDeg = c.courseDeg + angleChangeDeg }) cars


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharacterKey 'i' ->
            ( { model | viewCenterY = model.viewCenterY + 10 }, Cmd.none )

        CharacterKey 'm' ->
            ( { model | viewCenterY = model.viewCenterY - 10 }, Cmd.none )

        CharacterKey 'l' ->
            ( { model | viewCenterX = model.viewCenterX + 10 }, Cmd.none )

        CharacterKey 'j' ->
            ( { model | viewCenterX = model.viewCenterX - 10 }, Cmd.none )

        CharacterKey 'r' ->
            let
                updatedCars =
                    updateCars model.cars 10
            in
            ( { model | cars = updatedCars }, Cmd.none )

        TimeUpdate tposix ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 TimeUpdate, onKeyPress keyDecoder ]


view model =
    let
        pxWidth =
            String.fromInt model.viewWidthPixels

        pxHeight =
            String.fromInt model.viewHeightPixels

        roadEntities =
            List.append (renderViewBox model) (renderRoad model)

        carEntities =
            renderCars model
    in
    div []
        [ svg
            [ width pxWidth
            , height pxHeight
            , viewBox (String.join " " [ "0", "0", pxWidth, pxHeight ])
            ]
            (List.append carEntities roadEntities)
        ]


renderCars model =
    List.map (\c -> renderCar c model) model.cars


toRectangleXY car =
    let
        lowerLeft =
            getLowerLeftCorner car

        lowerRight =
            getLowerRightCorner car

        upperRight =
            getUpperRightCorner car

        upperLeft =
            getUpperLeftCorner car
    in
    [ lowerLeft, lowerRight, upperRight, upperLeft, lowerLeft ]


rotateBy point angleDeg =
    let
        theta =
            -1.0 * degrees angleDeg

        xr =
            cos theta * Tuple.first point - sin theta * Tuple.second point

        yr =
            sin theta * Tuple.first point + cos theta * Tuple.second point
    in
    ( xr, yr )



-- compute in car frame
-- rotate by course
-- translate to center


getLowerLeftCorner car =
    let
        dx =
            -1.0 * car.widthFeet / 2.0

        dy =
            -1.0 * car.lengthFeet / 2.0

        ( xr, yr ) =
            rotateBy ( dx, dy ) car.courseDeg
    in
    ( car.centerX + xr, car.centerY + yr )


getLowerRightCorner car =
    let
        dx =
            1.0 * car.widthFeet / 2.0

        dy =
            -1.0 * car.lengthFeet / 2.0

        ( xr, yr ) =
            rotateBy ( dx, dy ) car.courseDeg
    in
    ( car.centerX + xr, car.centerY + yr )


getUpperRightCorner car =
    let
        dx =
            1.0 * car.widthFeet / 2.0

        dy =
            1.0 * car.lengthFeet / 2.0

        ( xr, yr ) =
            rotateBy ( dx, dy ) car.courseDeg
    in
    ( car.centerX + xr, car.centerY + yr )


getUpperLeftCorner car =
    let
        dx =
            -1.0 * car.widthFeet / 2.0

        dy =
            1.0 * car.lengthFeet / 2.0

        ( xr, yr ) =
            rotateBy ( dx, dy ) car.courseDeg
    in
    ( car.centerX + xr, car.centerY + yr )


renderCar car model =
    let
        carBoundaryPointsXY =
            toRectangleXY car

        carBoundaryPointsPixels =
            List.map (\p -> toViewCoords2 p model) carBoundaryPointsXY

        carBoundaryPath =
            toBoundaryPath carBoundaryPointsPixels

        entity =
            Svg.path
                [ d (String.join " " carBoundaryPath)
                , stroke "gray"
                , fill "gray"
                , strokeWidth "2"
                , fillOpacity "0.5"
                ]
                []
    in
    entity


toBoundaryPath points =
    constructBoundaryPath points []


constructBoundaryPath points pathSoFar =
    case points of
        [] ->
            pathSoFar

        point :: rest ->
            case pathSoFar of
                [] ->
                    constructBoundaryPath rest [ "M", Tuple.first point, Tuple.second point ]

                _ ->
                    constructBoundaryPath rest (List.append pathSoFar [ "L", Tuple.first point, Tuple.second point ])


renderViewBox model =
    [ rect
        [ x "0"
        , y "0"
        , width (String.fromInt model.viewWidthPixels)
        , height (String.fromInt model.viewHeightPixels)
        , rx "0"
        , ry "0"
        , fill "#333333"
        , fillOpacity "0.1"
        ]
        []
    ]


renderRoad model =
    -- need to first compute road coordinates relative to the viewbox
    let
        upperLeftX =
            model.viewCenterX - model.viewWidthFeet / 2.0

        upperLeftY =
            model.viewCenterY + model.viewWidthFeet / 2.0

        entities =
            toSvg model ( upperLeftX, upperLeftY ) model.roadCenterline

        -- compute road coordinates relative to upper left corner
        --roadCoordsXY =
        --    List.map (\p -> translate p ( upperLeftX, upperLeftY )) model.roadCenterline
        -- now project endpoints of each segment perpendicular to centerline based on road width
        --edge1CoordsXY =
        --    projectEdges roadCoordsXY 90 25 []
        --edge2CoordsXY =
        --    projectEdges roadCoordsXY -90 25 []
        -- now convert relative to view box
        --roadCoordsPixels =
        --    List.map (\p -> toViewCoords p model) roadCoordsXY
        --
        -- edge1CoordsPixels =
        --     List.map (\p -> toViewCoords p model) edge1CoordsXY
        --
        -- edge2CoordsPixels =
        --     List.map (\p -> toViewCoords p model) edge2CoordsXY
        --
        -- centerLine =
        --     toSvgLines roadCoordsPixels []
        --
        -- edge1 =
        --     toSvgLines edge1CoordsPixels []
        --
        -- edge2 =
        --     toSvgLines edge2CoordsPixels []
    in
    entities



--    List.append edge2 (List.append centerLine edge1)


toSvg model upperLeftXY roadCenterline =
    renderSvg model upperLeftXY roadCenterline.originXY roadCenterline.originCourseDeg roadCenterline.segments []


renderSvg model upperLeftXY currentXY currentCourseDeg segmentsRemaining resultsSoFar =
    case segmentsRemaining of
        [] ->
            resultsSoFar

        first :: rest ->
            let
                -- TODO:  remove upperLeftXY as arg (can compute inside toViewCoords)
                ( entities, endXY, endCourseDeg ) =
                    renderSegment model upperLeftXY currentXY currentCourseDeg first
            in
            renderSvg model upperLeftXY endXY endCourseDeg rest (List.append entities resultsSoFar)


renderSegment model upperLeftXY currentXY currentCourseDeg segment =
    case segment of
        Linear lengthFeet courseDeg ->
            let
                ( center, nextXY ) =
                    renderLine model upperLeftXY currentXY lengthFeet courseDeg "yellow"

                -- in (X,Y) frame
                edge1CoordsXY =
                    projectEdge currentXY nextXY 90 25

                edge2CoordsXY =
                    projectEdge currentXY nextXY -90 25

                edge1CoordsPixels =
                    List.map (\p -> toViewCoords2 p model) edge1CoordsXY

                edge2CoordsPixels =
                    List.map (\p -> toViewCoords2 p model) edge2CoordsXY

                edge1 =
                    toSvgLinesWithColor edge1CoordsPixels [] "red"

                edge2 =
                    toSvgLinesWithColor edge2CoordsPixels [] "red"
            in
            --  ( [ center ], nextXY, courseDeg )
            ( center :: List.append edge1 edge2, nextXY, courseDeg )

        Arc lengthFeet angleChangeDeg ->
            let
                ( arcOriginXY, arcEndpointXY ) =
                    computeArcPoints currentXY currentCourseDeg angleChangeDeg lengthFeet

                -- turning right
                p1 =
                    translate currentXY upperLeftXY

                p2 =
                    translate arcEndpointXY upperLeftXY

                v1 =
                    toViewCoords p1 model

                v2 =
                    toViewCoords p2 model

                arcRadius =
                    scaleToView model lengthFeet

                sweepFlag =
                    getSweepFlag angleChangeDeg

                arcPath =
                    String.join " "
                        [ "M"
                        , Tuple.first v1
                        , Tuple.second v1
                        , "A"
                        , arcRadius
                        , arcRadius
                        , "0"
                        , "0"
                        , sweepFlag
                        , Tuple.first v2
                        , Tuple.second v2
                        ]

                entity =
                    Svg.path
                        [ d arcPath
                        , stroke "black"
                        , fill "white"
                        , strokeWidth "2"
                        , fillOpacity "0.0"
                        ]
                        []

                directionFactor =
                    getDirectionFactor angleChangeDeg

                -- now do the edges
                anglePlus =
                    currentCourseDeg + directionFactor * 90

                angleMinus =
                    currentCourseDeg - directionFactor * 90

                pointPlus =
                    projectPoint currentXY anglePlus 25.0

                pointMinus =
                    projectPoint currentXY angleMinus 25.0

                radiusPlus =
                    lengthFeet - 25.0

                radiusMinus =
                    lengthFeet + 25.0

                ( _, endpointPlusXY ) =
                    computeArcPoints pointPlus currentCourseDeg angleChangeDeg radiusPlus

                ( _, endpointMinusXY ) =
                    computeArcPoints pointMinus currentCourseDeg angleChangeDeg radiusMinus

                v1Plus =
                    toViewCoords2 pointPlus model

                v2Plus =
                    toViewCoords2 endpointPlusXY model

                v1Minus =
                    toViewCoords2 pointMinus model

                v2Minus =
                    toViewCoords2 endpointMinusXY model

                arcRadiusPlus =
                    scaleToView model (lengthFeet - 25.0)

                arcRadiusMinus =
                    scaleToView model (lengthFeet + 25.0)

                arcPathPlus =
                    String.join " "
                        [ "M"
                        , Tuple.first v1Plus
                        , Tuple.second v1Plus
                        , "A"
                        , arcRadiusPlus
                        , arcRadiusPlus
                        , "0"
                        , "0"
                        , sweepFlag
                        , Tuple.first v2Plus
                        , Tuple.second v2Plus
                        ]

                arcPathMinus =
                    String.join " "
                        [ "M"
                        , Tuple.first v1Minus
                        , Tuple.second v1Minus
                        , "A"
                        , arcRadiusMinus
                        , arcRadiusMinus
                        , "0"
                        , "0"
                        , sweepFlag
                        , Tuple.first v2Minus
                        , Tuple.second v2Minus
                        ]

                edgePlus =
                    Svg.path
                        [ d arcPathPlus
                        , stroke "black"
                        , fill "white"
                        , strokeWidth "2"
                        , fillOpacity "0.0"
                        ]
                        []

                edgeMinus =
                    Svg.path
                        [ d arcPathMinus
                        , stroke "black"
                        , fill "white"
                        , strokeWidth "2"
                        , fillOpacity "0.0"
                        ]
                        []
            in
            ( [ entity, edgePlus, edgeMinus ], arcEndpointXY, currentCourseDeg + angleChangeDeg )


getDirectionFactor : Float -> Float
getDirectionFactor angleChange =
    if angleChange > 0 then
        1.0

    else
        -1.0


getSweepFlag : Float -> String
getSweepFlag angleChange =
    if angleChange > 0 then
        "1"

    else
        "0"


renderLine model upperLeftXY currentXY lengthFeet courseDeg colorStr =
    let
        p1 =
            translate currentXY upperLeftXY

        dx =
            lengthFeet * sin (degrees courseDeg)

        dy =
            lengthFeet * cos (degrees courseDeg)

        p2 =
            ( Tuple.first p1 + dx, Tuple.second p1 + dy )

        nextXY =
            ( Tuple.first currentXY + dx, Tuple.second currentXY + dy )

        v1 =
            toViewCoords p1 model

        v2 =
            toViewCoords p2 model

        entity =
            toSvgLineWithColor v1 v2 colorStr
    in
    ( entity, nextXY )


scaleToView model distance =
    String.fromFloat (toFloat model.viewWidthPixels * distance / model.viewWidthFeet)


determineDirectionSign : Float -> Float
determineDirectionSign angleChangeDeg =
    if angleChangeDeg < 0 then
        -1.0

    else
        1.0


computeArcPoints xy courseDeg angleChangeDeg lengthFeet =
    let
        sign =
            determineDirectionSign angleChangeDeg

        dx =
            lengthFeet * sin (degrees (courseDeg + sign * 90))

        dy =
            lengthFeet * cos (degrees (courseDeg + sign * 90))

        originXY =
            ( Tuple.first xy + dx, Tuple.second xy + dy )

        -- compute course to the origin point
        dx1 =
            Tuple.first xy - Tuple.first originXY

        dy1 =
            Tuple.second xy - Tuple.second originXY

        courseToInitialPointOnArc =
            atan2 dx1 dy1

        -- apply arc change
        courseToFinalPointOnArc =
            courseToInitialPointOnArc + degrees angleChangeDeg

        -- find coord
        dx2 =
            lengthFeet * sin courseToFinalPointOnArc

        dy2 =
            lengthFeet * cos courseToFinalPointOnArc

        endXY =
            ( Tuple.first originXY + dx2, Tuple.second originXY + dy2 )
    in
    ( originXY, endXY )


projectEdges centerLineCoordsXY courseDeg distanceFeet edgesSoFar =
    case centerLineCoordsXY of
        [] ->
            edgesSoFar

        p1 :: rest ->
            case rest of
                [] ->
                    edgesSoFar

                p2 :: rest2 ->
                    projectEdges (List.drop 1 centerLineCoordsXY) courseDeg distanceFeet (List.append (projectEdge p1 p2 courseDeg distanceFeet) edgesSoFar)


projectEdge p1 p2 projectDeg distanceFeet =
    let
        segmentCourse =
            computeCourseDeg360 p1 p2

        perpCourse =
            limit360 (segmentCourse + projectDeg)

        n1 =
            projectPoint p1 perpCourse distanceFeet

        n2 =
            projectPoint p2 perpCourse distanceFeet
    in
    [ n1, n2 ]


computeCourseDeg360 p1 p2 =
    let
        dx =
            Tuple.first p2 - Tuple.first p1

        dy =
            Tuple.second p2 - Tuple.second p1

        courseDeg =
            180.0 / pi * atan2 dx dy
    in
    if courseDeg < 0 then
        360 + courseDeg

    else
        courseDeg


limit360 value =
    if value > 360 then
        value - 360

    else
        value


projectPoint : ( Float, Float ) -> Float -> Float -> ( Float, Float )
projectPoint point courseDeg distance =
    let
        dx =
            distance * sin (degrees courseDeg)

        dy =
            distance * cos (degrees courseDeg)
    in
    ( Tuple.first point + dx, Tuple.second point + dy )


translate : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
translate point origin =
    let
        dx =
            Tuple.first point - Tuple.first origin

        dy =
            Tuple.second point - Tuple.second origin
    in
    ( dx, dy )


toViewCoords : ( Float, Float ) -> Model -> ( String, String )
toViewCoords pointXY model =
    let
        px =
            Tuple.first pointXY / model.viewWidthFeet * toFloat model.viewWidthPixels

        py =
            Tuple.second pointXY / model.viewHeightFeet * toFloat model.viewHeightPixels
    in
    ( String.fromFloat px, String.fromFloat -py )


toViewCoords2 : ( Float, Float ) -> Model -> ( String, String )
toViewCoords2 pointXY model =
    let
        upperLeftX =
            model.viewCenterX - model.viewWidthFeet / 2.0

        upperLeftY =
            model.viewCenterY + model.viewWidthFeet / 2.0

        relativeToUpperLeft =
            translate pointXY ( upperLeftX, upperLeftY )

        px =
            Tuple.first relativeToUpperLeft / model.viewWidthFeet * toFloat model.viewWidthPixels

        py =
            Tuple.second relativeToUpperLeft / model.viewHeightFeet * toFloat model.viewHeightPixels
    in
    ( String.fromFloat px, String.fromFloat -py )


toSvgLines pointsRemaining lines =
    case pointsRemaining of
        [] ->
            lines

        p1 :: rest ->
            case rest of
                [] ->
                    lines

                p2 :: rest2 ->
                    toSvgLines (List.drop 1 pointsRemaining) (toSvgLine p1 p2 :: lines)


toSvgLinesWithColor pointsRemaining lines colorStr =
    case pointsRemaining of
        [] ->
            lines

        p1 :: rest ->
            case rest of
                [] ->
                    lines

                p2 :: rest2 ->
                    toSvgLinesWithColor (List.drop 1 pointsRemaining) (toSvgLineWithColor p1 p2 colorStr :: lines) colorStr


toSvgLine p1 p2 =
    line
        [ x1 (Tuple.first p1)
        , y1 (Tuple.second p1)
        , x2 (Tuple.first p2)
        , y2 (Tuple.second p2)
        , stroke "black"
        ]
        []


toSvgLineWithColor p1 p2 colorStr =
    line
        [ x1 (Tuple.first p1)
        , y1 (Tuple.second p1)
        , x2 (Tuple.first p2)
        , y2 (Tuple.second p2)
        , stroke colorStr
        ]
        []


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue
