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
    , isRunning : Bool
    , laneWidthFeet : Float
    , lanes : List Lane
    , epoch : Int
    }


type alias Lane =
    { id : String
    , segments : List LaneSegment
    }


type alias Radius =
    Float


type alias Angle =
    Float


type alias Point =
    ( Float, Float )


type LaneSegmentType
    = LinearLane
    | ArcLane Radius Angle


type alias LaneSegment =
    { startXY : Point
    , startCourseDeg : Float
    , endXY : Point
    , endCourseDeg : Float
    , laneID : String
    , segmentType : LaneSegmentType
    }


type alias Car =
    { id : String
    , colorStr : String
    , centerX : Float
    , centerY : Float
    , courseDeg : Float
    , widthFeet : Float
    , lengthFeet : Float
    , speedMph : Float
    , laneID : String
    , distance : Float
    , maneuver : List ( Float, Float )
    , maneuverDistance : Float
    }


type alias RoadCenterline =
    { originXY : ( Float, Float )
    , originCourseDeg : Float
    , segments : List Segment -- List (Float, Float)
    }


type Segment
    = Linear Float Float
    | Arc Float Float


type Renderable
    = RenderableArc ( Float, Float ) Float ( Float, Float ) Float String Float
    | RenderableLine ( Float, Float ) Float ( Float, Float ) Float


type Msg
    = TimeUpdate Time.Posix
    | CharacterKey Char
    | ControlKey String
    | Run
    | Stop


bezier startXY startCourse endXY endCourse controlOffset =
    let
        tValues =
            List.map (\n -> toFloat n / 100.0) (List.range 0 100)

        c1 =
            startXY

        c2 =
            projectPoint startXY startCourse controlOffset

        c3 =
            projectPoint endXY (endCourse + 180) controlOffset

        c4 =
            endXY
    in
    List.map (\t -> connectBezier [ ( c1, c2 ), ( c2, c3 ), ( c3, c4 ) ] t) tValues


connectBezier segments t =
    if List.length segments == 1 then
        case List.head segments of
            Nothing ->
                ( 0.0, 0.0 )

            Just aSegment ->
                findPointOnSegment aSegment t

    else
        let
            nextSegments =
                connectSegments segments t []
        in
        connectBezier nextSegments t


connectSegments segments t resultsSoFar =
    case segments of
        first :: rest ->
            case rest of
                next :: rest2 ->
                    let
                        p1 =
                            findPointOnSegment first t

                        p2 =
                            findPointOnSegment next t
                    in
                    connectSegments (List.drop 1 segments) t (List.reverse (( p1, p2 ) :: resultsSoFar))

                [] ->
                    resultsSoFar

        [] ->
            resultsSoFar


findPointOnSegment : ( ( Float, Float ), ( Float, Float ) ) -> Float -> ( Float, Float )
findPointOnSegment ( from, to ) t =
    let
        dx =
            Tuple.first to - Tuple.first from

        dy =
            Tuple.second to - Tuple.second from
    in
    ( Tuple.first from + t * dx, Tuple.second from + t * dy )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        centerline =
            RoadCenterline ( 100.0, 100.0 ) 0.0 [ Linear 100.0 0.0, Arc 50.0 30.0, Linear 100.0 30.0, Arc 50.0 -30.0, Linear 100.0 0.0 ]

        cars =
            initCars

        lanes =
            initLanes centerline 15.0
    in
    --    ( Model 100.0 100.0 200.0 200.0 800 800 (RoadCenterline ( 100.0, 100.0 ) 0.0 [ Linear 500.0 0.0 ]) initCars False, Cmd.none )
    ( Model 100.0 100.0 200.0 200.0 800 800 centerline cars False 15.0 lanes 0, Cmd.none )


initCars =
    [ Car "1" "gray" 107.5 100.0 0.0 10.0 20.0 10.0 "Right" 0.0 [] 0.0, Car "2" "yellow" 92.5 110.0 0.0 10.0 20.0 12.0 "Left" 10.0 [] 0.0 ]



--


initLanes centerline laneWidthFeet =
    let
        originLeftXY =
            shift { startXY = centerline.originXY, startCourseDeg = centerline.originCourseDeg } (-laneWidthFeet / 2.0)

        originRightXY =
            shift { startXY = centerline.originXY, startCourseDeg = centerline.originCourseDeg } (laneWidthFeet / 2.0)

        originCourse =
            centerline.originCourseDeg

        leftLaneSegments =
            generateLaneSegments { startXY = originLeftXY, startCourseDeg = centerline.originCourseDeg } centerline.segments [] "Left"

        rightLaneSegments =
            generateLaneSegments { startXY = originRightXY, startCourseDeg = centerline.originCourseDeg } centerline.segments [] "Right"
    in
    [ Lane "Left" leftLaneSegments, Lane "Right" rightLaneSegments ]


generateLaneSegments origin centerlineSegmentsRemaining laneSegmentsSoFar laneID =
    case centerlineSegmentsRemaining of
        [] ->
            List.reverse laneSegmentsSoFar

        first :: rest ->
            let
                laneSegment =
                    generateLaneSegment first origin laneID
            in
            generateLaneSegments { startXY = laneSegment.endXY, startCourseDeg = laneSegment.endCourseDeg } rest (laneSegment :: laneSegmentsSoFar) laneID



--Arc 50.0 -90.0 ]), Cmd.none )
--[ ( 100.0, 0.0 ), ( 100.0, 1000.0 ), ( 100.0, 2000.0 ) ], Cmd.none )


generateLaneSegment centerlineSegment origin laneID =
    let
        ( endXY, endCourseDeg ) =
            determineEndConditions centerlineSegment origin.startXY origin.startCourseDeg
    in
    case centerlineSegment of
        Linear lengthFeet courseDeg ->
            LaneSegment origin.startXY origin.startCourseDeg endXY endCourseDeg laneID LinearLane

        Arc radiusFeet angleChangeDeg ->
            LaneSegment origin.startXY origin.startCourseDeg endXY endCourseDeg laneID (ArcLane radiusFeet angleChangeDeg)


rotateCars cars angleChangeDeg =
    List.map (\c -> { c | courseDeg = c.courseDeg + angleChangeDeg }) cars


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model | isRunning = True }, Cmd.none )

        Stop ->
            ( { model | isRunning = False }, Cmd.none )

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
                    rotateCars model.cars 10
            in
            ( { model | cars = updatedCars }, Cmd.none )

        TimeUpdate tposix ->
            if model.isRunning then
                let
                    updatedCars =
                        updateCarsInLanes model.cars model.lanes

                    updatedCars2 =
                        generateLaneChanges updatedCars model.lanes model.epoch
                in
                ( { model
                    | cars = updatedCars2
                    , epoch = model.epoch + 1
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


generateLaneChanges cars lanes epoch =
    List.map (\c -> generateLaneChange c lanes epoch) cars


generateLaneChange car lanes epoch =
    if epoch == 10 then
        let
            lane =
                getCurrentLane car lanes

            other =
                getOtherLane car lanes
        in
        case lane of
            Just currentLane ->
                case other of
                    Just otherLane ->
                        let
                            maneuver =
                                constructLaneChange car currentLane otherLane
                        in
                        { car | maneuver = maneuver }

                    Nothing ->
                        car

            Nothing ->
                car

    else
        car


constructLaneChange car currentLane otherLane =
    let
        startXY =
            ( car.centerX, car.centerY )

        startCourse =
            car.courseDeg

        ( maybeEndXY, endCourse ) =
            projectInLane car.distance 50.0 otherLane
    in
    case maybeEndXY of
        Nothing ->
            []

        Just endXY ->
            bezier startXY startCourse endXY endCourse 25.0


updateCarsInLanes cars lanes =
    List.map (\c -> updateCarInLane c lanes) cars


getCurrentLane car lanes =
    let
        current =
            List.filter (\lane -> car.laneID == lane.id) lanes
    in
    List.head current


getOtherLane car lanes =
    let
        target =
            List.filter (\lane -> not (car.laneID == lane.id)) lanes
    in
    List.head target


updateCarInLane car lanes =
    case car.maneuver of
        [] ->
            let
                lane =
                    getCurrentLane car lanes
            in
            case lane of
                Nothing ->
                    car

                Just aLane ->
                    let
                        posnChange =
                            car.speedMph * 3600.0 / 5280.0

                        ( updatedPosnXY, updatedCourseDeg ) =
                            projectInLane car.distance posnChange aLane

                        updatedDistance =
                            car.distance + posnChange
                    in
                    case updatedPosnXY of
                        Nothing ->
                            car

                        Just aPosn ->
                            { car
                                | centerX = Tuple.first aPosn
                                , centerY = Tuple.second aPosn
                                , distance = updatedDistance
                                , courseDeg = updatedCourseDeg
                            }

        maneuverPoints ->
            let
                laneTarget =
                    getOtherLane car lanes

                maneuverSegments =
                    toManeuverSegments maneuverPoints []
            in
            case laneTarget of
                Nothing ->
                    car

                Just aLaneTarget ->
                    let
                        maneuverLength =
                            computeManeuverLength maneuverSegments

                        stepFeet =
                            car.speedMph * 3600.0 / 5280.0
                    in
                    if (car.maneuverDistance + stepFeet) >= maneuverLength then
                        let
                            stepToUse =
                                maneuverLength - car.maneuverDistance

                            updatedPosn1 =
                                getPosnOnManeuver maneuverSegments (car.maneuverDistance + stepToUse)

                            ( updatedPosn, updatedDistanceOnLane, updatedCourse ) =
                                projectOntoLane updatedPosn1 aLaneTarget

                            -- take the rest of the step
                            stepRemaining =
                                car.maneuverDistance + stepFeet - maneuverLength

                            ( updatedPosn2, updatedCourseDeg2 ) =
                                projectInLane updatedDistanceOnLane stepRemaining aLaneTarget

                            --                            updatedPosn2 =
                            --                                projectPoint updatedPosn updatedCourse stepRemaining
                            updatedDistanceOnLane2 =
                                updatedDistanceOnLane + stepRemaining

                            --updatedManeuverDistance =
                            --    car.maneuverDistance + stepToUse
                        in
                        case updatedPosn2 of
                            Nothing ->
                                let
                                    updatedPosn2b =
                                        projectPoint updatedPosn updatedCourse stepRemaining

                                    updatedCourseDeg2b =
                                        updatedCourse
                                in
                                { car
                                    | centerX = Tuple.first updatedPosn2b
                                    , centerY = Tuple.second updatedPosn2b
                                    , distance = updatedDistanceOnLane2
                                    , courseDeg = updatedCourseDeg2b
                                    , maneuverDistance = 0 --car.maneuverDistance + stepToUse
                                    , laneID = aLaneTarget.id
                                    , maneuver = []
                                }

                            Just updatedPosnXY ->
                                { car
                                    | centerX = Tuple.first updatedPosnXY
                                    , centerY = Tuple.second updatedPosnXY
                                    , distance = updatedDistanceOnLane2
                                    , courseDeg = updatedCourseDeg2
                                    , maneuverDistance = 0 --car.maneuverDistance + stepToUse
                                    , laneID = aLaneTarget.id
                                    , maneuver = []
                                }

                    else
                        let
                            courseOnManeuver =
                                getCourseOnManeuver maneuverSegments (car.maneuverDistance + stepFeet)

                            updatedPosn =
                                getPosnOnManeuver maneuverSegments (car.maneuverDistance + stepFeet)

                            updatedManeuverDistance =
                                car.maneuverDistance + stepFeet

                            updatedManeuver =
                                car.maneuver
                        in
                        { car
                            | centerX = Tuple.first updatedPosn
                            , centerY = Tuple.second updatedPosn
                            , courseDeg = courseOnManeuver
                            , maneuverDistance = car.maneuverDistance + stepFeet
                        }


toManeuverSegments pointsRemaining resultsSoFar =
    case pointsRemaining of
        [] ->
            List.reverse resultsSoFar

        first :: rest ->
            case rest of
                next :: rest2 ->
                    toManeuverSegments (List.drop 1 pointsRemaining) (( first, next ) :: resultsSoFar)

                [] ->
                    List.reverse resultsSoFar


projectOntoLane posn lane =
    let
        maybeContainingSegment =
            findSegmentContaining posn lane
    in
    case maybeContainingSegment of
        Nothing ->
            ( ( 0.0, 0.0 ), 0.0, 0.0 )

        Just containingSegment ->
            ( containingSegment.posnOnSegmentXY, containingSegment.distanceOnLane, containingSegment.courseOnSegmentDeg )


projectOntoLaneSegment : LaneSegment -> ( Float, Float ) -> ( ( Float, Float ), Float )
projectOntoLaneSegment segment posn =
    case segment.segmentType of
        LinearLane ->
            let
                dToPoint =
                    computeDistance (Just segment.startXY) (Just posn)

                courseToPoint =
                    computeCourseDeg360 segment.startXY posn

                relativeCourse =
                    courseToPoint - segment.endCourseDeg

                projectedDistance =
                    dToPoint * cos (degrees relativeCourse)

                dx =
                    projectedDistance * sin (degrees segment.endCourseDeg)

                dy =
                    projectedDistance * cos (degrees segment.endCourseDeg)

                projectedPosn =
                    ( Tuple.first segment.startXY + dx, Tuple.second segment.startXY + dy )
            in
            ( projectedPosn, segment.endCourseDeg )

        ArcLane radiusFeet angleChangeDeg ->
            let
                ( centerXY, endXY ) =
                    computeArcPoints segment.startXY segment.startCourseDeg angleChangeDeg radiusFeet

                courseToPoint =
                    computeCourseDeg360 centerXY posn

                pointOnArc =
                    projectPoint centerXY courseToPoint radiusFeet

                courseToStart =
                    computeCourseDeg360 centerXY segment.startXY

                theta =
                    courseToPoint - courseToStart

                courseAtPoint =
                    segment.startCourseDeg + theta
            in
            ( pointOnArc, courseAtPoint )


recursiveFindSegmentContaining posn segmentsRemaining distanceSoFar resultsSoFar =
    case segmentsRemaining of
        [] ->
            resultsSoFar

        first :: rest ->
            if laneSegmentContains first posn then
                let
                    ( posnXY, courseDeg ) =
                        projectOntoLaneSegment first posn

                    d =
                        computeDistance (Just first.startXY) (Just posnXY)

                    result =
                        { segment = first, posnOnSegmentXY = posnXY, courseOnSegmentDeg = courseDeg, distanceOnLane = distanceSoFar + d }
                in
                recursiveFindSegmentContaining posn (List.drop 1 segmentsRemaining) (distanceSoFar + d) (result :: resultsSoFar)

            else
                let
                    d =
                        computeLength first
                in
                recursiveFindSegmentContaining posn (List.drop 1 segmentsRemaining) (distanceSoFar + d) resultsSoFar


findSegmentContaining posn lane =
    let
        contains =
            recursiveFindSegmentContaining posn lane.segments 0.0 []
    in
    List.head contains


laneSegmentContains segment posn =
    case segment.segmentType of
        LinearLane ->
            let
                distanceToPoint =
                    computeDistance (Just segment.startXY) (Just posn)

                courseToPoint =
                    180.0 / pi * computeCourse (Just segment.startXY) (Just posn)

                relativeCourse =
                    segment.endCourseDeg - courseToPoint

                distanceOnSegment =
                    distanceToPoint * cos (degrees relativeCourse)

                segmentLength =
                    computeLength segment
            in
            distanceOnSegment >= 0 && distanceOnSegment <= segmentLength

        ArcLane radiusFeet angleChangeDeg ->
            let
                ( centerXY, endXY ) =
                    computeArcPoints segment.startXY segment.startCourseDeg angleChangeDeg radiusFeet

                r1 =
                    radiusFeet - 25.0

                r2 =
                    radiusFeet + 25.0

                distanceFromOrigin =
                    computeDistance (Just centerXY) (Just posn)

                courseToPointDeg =
                    computeCourseDeg360 centerXY posn

                course1 =
                    computeCourseDeg360 centerXY segment.startXY

                course2 =
                    computeCourseDeg360 centerXY endXY
            in
            withinDistance r1 r2 distanceFromOrigin && withinAzimuth (limit360 course1) (limit360 course2) courseToPointDeg


getPosnOnManeuver maneuver distance =
    let
        ( maybeSegment, courseDeg, distanceOnSegment ) =
            recursiveFindManeuverSegmentAtDistance 0.0 distance maneuver
    in
    case maybeSegment of
        Nothing ->
            ( 0.0, 0.0 )

        Just aSegment ->
            projectPoint (Tuple.first aSegment) courseDeg distanceOnSegment


getCourseOnManeuver maneuver distance =
    let
        ( maybeSegment, courseOnSegment, distanceOnSegment ) =
            recursiveFindManeuverSegmentAtDistance 0.0 distance maneuver
    in
    case maybeSegment of
        Nothing ->
            0.0

        Just aSegment ->
            let
                from =
                    Tuple.first aSegment

                to =
                    Tuple.second aSegment

                courseDeg =
                    computeCourseDeg360 from to
            in
            courseDeg


recursiveFindManeuverSegmentAtDistance distanceAtStart targetDistance segmentsRemaining =
    case segmentsRemaining of
        [] ->
            ( Nothing, 0.0, 0.0 )

        first :: rest ->
            let
                distanceAtEnd =
                    distanceAtStart + computeManeuverSegmentLength first

                courseOnSegment =
                    computeCourse (Just (Tuple.first first)) (Just (Tuple.second first))
            in
            if distanceAtEnd >= targetDistance then
                ( Just first, courseOnSegment, targetDistance - distanceAtStart )

            else
                recursiveFindManeuverSegmentAtDistance distanceAtEnd targetDistance (List.drop 1 segmentsRemaining)


computeManeuverSegmentLength maneuverSegment =
    let
        from =
            Tuple.first maneuverSegment

        to =
            Tuple.second maneuverSegment

        dx =
            Tuple.first to - Tuple.first from

        dy =
            Tuple.second to - Tuple.second from
    in
    sqrt (dx * dx + dy * dy)


computeManeuverLength segments =
    recursiveComputeManeuverLength segments 0.0


recursiveComputeManeuverLength segmentsRemaining distanceSoFar =
    case segmentsRemaining of
        [] ->
            distanceSoFar

        first :: rest ->
            let
                from =
                    Tuple.first first

                to =
                    Tuple.second first

                dx =
                    Tuple.first to - Tuple.first from

                dy =
                    Tuple.second to - Tuple.second from

                dd =
                    sqrt (dx * dx + dy * dy)
            in
            recursiveComputeManeuverLength (List.drop 1 segmentsRemaining) (distanceSoFar + dd)


projectInLane fromDistance posnChange lane =
    let
        ( segment, distanceOnSegment ) =
            findSegmentAtDistance (fromDistance + posnChange) lane
    in
    case segment of
        Nothing ->
            ( Nothing, 0 )

        Just aLaneSegment ->
            case aLaneSegment.segmentType of
                ArcLane radiusFeet angleChangeDeg ->
                    let
                        ( centerXY, endXY ) =
                            computeArcPoints aLaneSegment.startXY aLaneSegment.startCourseDeg angleChangeDeg radiusFeet

                        directionFactor =
                            getDirectionFactor angleChangeDeg

                        theta =
                            directionFactor * distanceOnSegment / radiusFeet

                        courseToStartPoint =
                            computeCourseDeg360 centerXY aLaneSegment.startXY

                        courseToUpdatedPosn =
                            courseToStartPoint + (180 / pi * theta)
                    in
                    ( Just (projectPoint centerXY courseToUpdatedPosn radiusFeet), aLaneSegment.startCourseDeg + 180.0 / pi * theta )

                LinearLane ->
                    ( Just (projectPoint aLaneSegment.startXY aLaneSegment.endCourseDeg distanceOnSegment), aLaneSegment.endCourseDeg )


recursiveFindSegmentAtDistance : Float -> Float -> List LaneSegment -> ( Maybe LaneSegment, Float )
recursiveFindSegmentAtDistance distanceAtStart targetDistance segmentsRemaining =
    case segmentsRemaining of
        [] ->
            ( Nothing, 0.0 )

        first :: rest ->
            let
                distanceAtEnd =
                    distanceAtStart + computeLength first
            in
            if distanceAtEnd >= targetDistance then
                ( Just first, targetDistance - distanceAtStart )

            else
                recursiveFindSegmentAtDistance distanceAtEnd targetDistance (List.drop 1 segmentsRemaining)


computeLength segment =
    case segment.segmentType of
        ArcLane radiusFeet angleChangeDeg ->
            radiusFeet * abs (degrees angleChangeDeg)

        LinearLane ->
            computeDistance (Just segment.startXY) (Just segment.endXY)


findSegmentAtDistance : Float -> Lane -> ( Maybe LaneSegment, Float )
findSegmentAtDistance distance lane =
    recursiveFindSegmentAtDistance 0.0 distance lane.segments


updateCars cars roadCenterline =
    List.map (\c -> updateCar c roadCenterline) cars



-- need to associate each car with a Lane


toCoords segment originXY =
    case segment of
        Linear lengthFeet courseDeg ->
            let
                dx =
                    lengthFeet * sin (degrees courseDeg)

                dy =
                    lengthFeet * cos (degrees courseDeg)

                nextX =
                    Tuple.first originXY + dx

                nextY =
                    Tuple.second originXY + dy
            in
            [ originXY, ( nextX, nextY ) ]

        _ ->
            []


type AbsoluteSegment
    = AbsoluteSegment
        { descriptor : Segment
        , startXY : ( Float, Float )
        , startCourseDeg : Float
        , endXY : ( Float, Float )
        , endCourseDeg : Float
        , next : Maybe AbsoluteSegment
        }


toSegmentRecord segment remaining currentXY currentCourseDeg =
    case segment of
        Linear lengthFeet courseDeg ->
            let
                dx =
                    lengthFeet * sin (degrees courseDeg)

                dy =
                    lengthFeet * cos (degrees courseDeg)

                nextXY =
                    ( Tuple.first currentXY + dx, Tuple.second currentXY + dy )
            in
            { descriptor = segment
            , startXY = currentXY
            , startCourseDeg = currentCourseDeg
            , endXY = nextXY
            , endCourseDeg = courseDeg
            }

        Arc lengthFeet angleChangeDeg ->
            let
                ( arcOriginXY, arcEndpointXY ) =
                    computeArcPoints currentXY currentCourseDeg angleChangeDeg lengthFeet
            in
            { descriptor = segment
            , startXY = currentXY
            , startCourseDeg = currentCourseDeg
            , endXY = arcEndpointXY
            , endCourseDeg = currentCourseDeg + angleChangeDeg
            }


recursiveLink remaining resultsSoFar =
    case remaining of
        [] ->
            List.reverse resultsSoFar

        first :: rest ->
            let
                updated =
                    ( first, List.head rest )
            in
            recursiveLink (List.drop 1 remaining) (updated :: resultsSoFar)


linkRecords records =
    recursiveLink records []


recursiveGenerateSegments segmentsToGo currentXY currentCourseDeg resultsSoFar =
    case segmentsToGo of
        [] ->
            let
                linkedRecords =
                    linkRecords (List.reverse resultsSoFar)
            in
            linkedRecords

        first :: rest ->
            let
                segmentRecord =
                    toSegmentRecord first rest currentXY currentCourseDeg
            in
            recursiveGenerateSegments (List.drop 1 segmentsToGo) segmentRecord.endXY segmentRecord.endCourseDeg (segmentRecord :: resultsSoFar)


toAbsoluteSegments roadCenterline =
    recursiveGenerateSegments roadCenterline.segments roadCenterline.originXY roadCenterline.originCourseDeg []


determineTargetSegment car roadCenterline =
    let
        segments =
            toAbsoluteSegments roadCenterline

        contains =
            List.filter (\s -> segmentContains s car) segments

        crossTrackSegments =
            List.map (\c -> constructCrossTrackSegment c car) contains

        sortedSegments =
            List.sortBy .crossTrackFeet crossTrackSegments

        closest =
            List.head sortedSegments
    in
    case closest of
        Just aCrossTrackRecord ->
            let
                linkedRecord =
                    aCrossTrackRecord.record

                record =
                    Tuple.first linkedRecord
            in
            case record.descriptor of
                Linear _ _ ->
                    Just record

                Arc _ _ ->
                    let
                        nextRecord =
                            Tuple.second linkedRecord
                    in
                    nextRecord

        Nothing ->
            Nothing


constructCrossTrackSegment linkedRecord car =
    let
        segmentRecord =
            Tuple.first linkedRecord
    in
    case segmentRecord.descriptor of
        Linear lengthFeet courseDeg ->
            let
                crossTrackFeet =
                    computeCrossTrackError segmentRecord.startXY courseDeg ( car.centerX, car.centerY )
            in
            { record = linkedRecord
            , crossTrackFeet = abs crossTrackFeet
            }

        Arc lengthFeet angleChangeDeg ->
            let
                startXY =
                    segmentRecord.startXY

                startCourseDeg =
                    segmentRecord.startCourseDeg

                ( arcOriginXY, arcEndpointXY ) =
                    computeArcPoints startXY startCourseDeg angleChangeDeg lengthFeet

                distanceFromOrigin =
                    computeDistance (Just arcOriginXY) (Just ( car.centerX, car.centerY ))
            in
            { record = linkedRecord
            , crossTrackFeet = abs (lengthFeet - distanceFromOrigin)
            }


segmentContains linkedRecord car =
    let
        segmentRecord =
            Tuple.first linkedRecord
    in
    case segmentRecord.descriptor of
        Linear lengthFeet courseDeg ->
            let
                projectionFeet =
                    computeProjection segmentRecord.startXY courseDeg ( car.centerX, car.centerY )
            in
            projectionFeet > 0 && projectionFeet <= lengthFeet

        Arc lengthFeet angleChangeDeg ->
            let
                startXY =
                    segmentRecord.startXY

                startCourseDeg =
                    segmentRecord.startCourseDeg

                ( arcOriginXY, arcEndpointXY ) =
                    computeArcPoints startXY startCourseDeg angleChangeDeg lengthFeet

                -- is point within a given distance and course range of arc originXY
                r1 =
                    lengthFeet - 25.0

                r2 =
                    lengthFeet + 25.0

                distanceFromOrigin =
                    computeDistance (Just arcOriginXY) (Just ( car.centerX, car.centerY ))

                courseToPointDeg =
                    computeCourseDeg360 arcOriginXY ( car.centerX, car.centerY )

                course1 =
                    computeCourseDeg360 arcOriginXY startXY

                course2 =
                    computeCourseDeg360 arcOriginXY arcEndpointXY
            in
            withinDistance r1 r2 distanceFromOrigin && withinAzimuth (limit360 course1) (limit360 course2) courseToPointDeg


withinDistance dmin dmax d =
    dmin <= d && d <= dmax


withinAzimuth az1 az2 az =
    if az1 < az2 then
        az1 <= az && az <= az2

    else
        az2 <= az && az <= az1


computeProjection originXY courseDeg pointXY =
    let
        distanceToPoint =
            computeDistance (Just originXY) (Just pointXY)

        courseToPointDeg =
            computeCourseDeg360 originXY pointXY

        relativeCourseDeg =
            courseToPointDeg - courseDeg
    in
    distanceToPoint * cos (degrees relativeCourseDeg)


computeCrossTrackError originXY courseDeg pointXY =
    let
        distanceToPoint =
            computeDistance (Just originXY) (Just pointXY)

        courseToPointDeg =
            computeCourseDeg360 originXY pointXY

        relativeCourseDeg =
            courseToPointDeg - courseDeg
    in
    distanceToPoint * sin (degrees relativeCourseDeg)


computeSteeringCommand car roadCenterline =
    let
        targetSegment =
            determineTargetSegment car roadCenterline
    in
    case targetSegment of
        Nothing ->
            0.0

        Just aSegment ->
            case aSegment.descriptor of
                Linear lengthFeet courseDeg ->
                    let
                        -- project onto segment
                        coords =
                            toCoords aSegment.descriptor aSegment.startXY

                        dToPoint =
                            computeDistance (List.head coords) (Just ( car.centerX, car.centerY ))

                        _ =
                            Debug.log "Distance to point: " dToPoint

                        courseToPoint =
                            computeCourse (List.head coords) (Just ( car.centerX, car.centerY ))

                        _ =
                            Debug.log "Course to point: " courseToPoint

                        courseOnSegment =
                            computeCourse (List.head coords) (List.head (List.drop 1 coords))

                        _ =
                            Debug.log "Segment course: " courseOnSegment

                        deltaCourse =
                            courseToPoint - courseOnSegment

                        _ =
                            Debug.log "Delta course: " deltaCourse

                        dOnSegment =
                            dToPoint * cos deltaCourse

                        _ =
                            Debug.log "Distance on segment: " dOnSegment

                        targetDistance =
                            dOnSegment + 50

                        targetPoint =
                            projectOnSegment coords targetDistance

                        _ =
                            Debug.log "Target point on segment: " targetPoint

                        dx =
                            Tuple.first targetPoint - car.centerX

                        dy =
                            Tuple.second targetPoint - car.centerY

                        _ =
                            Debug.log "dx: " dx

                        _ =
                            Debug.log "dy: " dy

                        targetCourseDeg =
                            180 / pi * atan2 dx dy
                    in
                    targetCourseDeg - car.courseDeg

                Arc radius angleChangeDeg ->
                    0.0


recursiveProject coordsRemaining targetDistance distanceSoFar currentXY currentCourse =
    case coordsRemaining of
        [] ->
            if distanceSoFar < targetDistance then
                let
                    dToGo =
                        targetDistance - distanceSoFar

                    dx =
                        dToGo * sin currentCourse

                    dy =
                        dToGo * cos currentCourse

                    nextX =
                        Tuple.first currentXY + dx

                    nextY =
                        Tuple.second currentXY + dy
                in
                ( nextX, nextY )

            else
                currentXY

        first :: rest ->
            let
                dOnSegment =
                    computeDistance (Just currentXY) (Just first)

                courseOnSegment =
                    computeCourse (Just currentXY) (Just first)

                dAfterSegment =
                    distanceSoFar + dOnSegment
            in
            if dAfterSegment > targetDistance then
                let
                    dToGo =
                        targetDistance - distanceSoFar

                    dx =
                        dToGo * sin courseOnSegment

                    dy =
                        dToGo * cos courseOnSegment

                    nextX =
                        Tuple.first currentXY + dx

                    nextY =
                        Tuple.second currentXY + dy
                in
                ( nextX, nextY )

            else
                recursiveProject (List.drop 1 coordsRemaining) targetDistance dAfterSegment first courseOnSegment


projectOnSegment coords targetDistance =
    let
        currentCourse =
            computeCourse (List.head coords) (List.head (List.drop 1 coords))
    in
    case List.head coords of
        Nothing ->
            ( 0.0, 0.0 )

        Just aCoord ->
            recursiveProject (List.drop 1 coords) targetDistance 0.0 aCoord currentCourse



-- let
--
--
--     --segments = toAbsoluteSegments roadCenterline
--
--     --( currentSegment, currentDistance, nextSegment ) =
--     --    mapToRoad ( car.centerX, car.centerY ) segments
--
--     --steeringTarget =
--     --    computeSteeringTarget currentSegment currentDistance nextSegment
--
--     -- compute steering command to aim at steering target
-- in
-- steeringTarget - car.courseDeg
-- getLastSegment segments =
--   case segments of
--     first :: rest ->
--       case rest of
--         Nothing ->
--           first
--         Just aList ->
--           getLastSegment rest
--     Nothing ->
--       Nothing
--
--
-- sumLengthOver segments distanceSoFar =
--   case segments of
--     [] ->
--       distanceSoFar
--     first :: rest ->
--       sumLengthOver rest (distanceSoFar + first.length)
--


computeDistance from to =
    case from of
        Nothing ->
            0.0

        Just apoint ->
            case to of
                Nothing ->
                    0.0

                Just apoint2 ->
                    let
                        dx =
                            Tuple.first apoint2 - Tuple.first apoint

                        dy =
                            Tuple.second apoint2 - Tuple.second apoint
                    in
                    sqrt (dx * dx + dy * dy)


computeCourse from to =
    case from of
        Nothing ->
            0.0

        Just apoint ->
            case to of
                Nothing ->
                    0.0

                Just apoint2 ->
                    let
                        dx =
                            Tuple.first apoint2 - Tuple.first apoint

                        dy =
                            Tuple.second apoint2 - Tuple.second apoint
                    in
                    atan2 dx dy



--
-- getLastPointOnSegment segment =
--   case segment of
--     Nothing ->
--       Nothing
--     Just asegment ->
--       getLastPoint asegment.points
--
-- getLastPoint points =
--   case points of
--     first :: rest ->
--       case rest of
--         Nothing ->
--           first
--         _ ->
--           getLastPoint rest
--     [] ->
--       Nothing
--
-- projectAlong dToPoint courseToPoint
--
-- -- mapToRoad centerXY segments =
-- --   let
-- --     contains = List.filter (\s -> segmentContains centerXY s) segments
-- --   in
-- --     case contains of
-- --       [] ->
-- --         let
-- --           lastSegment = getLastSegment segments
-- --           distance = sumLengthOver segments 0.0
-- --           lastPoint = getLastPointOnSegment lastSegment
-- --           dToPoint = computeDistance lastPoint centerXY
-- --           courseToPoint = computeCourse lastPoint centerXY
-- --           dAlongProjection = projectAlong dToPoint courseToPoint last
-- --         in
-- --         -- extend LAST segment
-- --         (lastSegment, distance + dAlongProjection, Nothing)
-- --
-- --       first :: rest ->
-- --         case rest of
-- --           [] ->
-- --
-- --           first2 :: rest2 ->
--


updateCar car roadCenterline =
    let
        steeringCommand =
            computeSteeringCommand car roadCenterline

        _ =
            Debug.log "Steering command (Deg)" steeringCommand

        speedFeetPerSecond =
            car.speedMph * 3600.0 / 5280.0

        -- compute center of instantaneous steering circle formed by intersection of front wheel and back wheels
        turnRateRadiansPerSec =
            speedFeetPerSecond / car.lengthFeet * tan (degrees steeringCommand)

        dx =
            speedFeetPerSecond * sin (degrees car.courseDeg)

        dy =
            speedFeetPerSecond * cos (degrees car.courseDeg)

        deltaCourseDeg =
            turnRateRadiansPerSec * 180 / pi
    in
    { car | centerX = car.centerX + dx, centerY = car.centerY + dy, courseDeg = car.courseDeg + deltaCourseDeg }


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

        laneEntities =
            renderLanes2 model "blue"

        carEntities =
            List.concat (renderCars model)
    in
    div []
        [ div []
            [ button [ onClick Run ] [ Html.text "Run" ]
            , button [ onClick Stop ] [ Html.text "Stop" ]
            ]
        , div []
            [ svg
                [ width pxWidth
                , height pxHeight
                , viewBox (String.join " " [ "0", "0", pxWidth, pxHeight ])
                ]
                (List.append carEntities laneEntities)
            ]
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
        _ =
            Debug.log "EPOCH" model.epoch

        _ =
            Debug.log "Car" car.id

        _ =
            Debug.log "Posn" ( car.centerX, car.centerY )

        _ =
            Debug.log "Lane" car.laneID

        carBoundaryPointsXY =
            toRectangleXY car

        carBoundaryPointsPixels =
            List.map (\p -> toViewCoords2 p model) carBoundaryPointsXY

        carBoundaryPath =
            toBoundaryPath carBoundaryPointsPixels

        entity =
            Svg.path
                [ d (String.join " " carBoundaryPath)
                , stroke car.colorStr
                , fill car.colorStr
                , strokeWidth "2"
                , fillOpacity "0.5"
                ]
                []

        maneuverPath =
            constructManeuverPath car model
    in
    entity :: maneuverPath


constructManeuverPath car model =
    case car.maneuver of
        [] ->
            let
                v1 =
                    toViewCoords2 ( car.centerX, car.centerY ) model
            in
            [ circle
                [ cx (Tuple.first v1)
                , cy (Tuple.second v1)
                , r "5"
                , fill "#ff0000"
                , fillOpacity "0.5"
                ]
                []
            ]

        segments ->
            renderManeuverSegments segments model []


renderManeuverSegments segmentsRemaining model resultsSoFar =
    case segmentsRemaining of
        [] ->
            resultsSoFar

        first :: rest ->
            case rest of
                next :: rest2 ->
                    let
                        entity =
                            renderManeuverSegment ( first, next ) model
                    in
                    renderManeuverSegments (List.drop 1 segmentsRemaining) model (entity :: resultsSoFar)

                [] ->
                    resultsSoFar


renderManeuverSegment segment model =
    let
        v1 =
            toViewCoords2 (Tuple.first segment) model

        v2 =
            toViewCoords2 (Tuple.second segment) model
    in
    toSvgLineWithColor v1 v2 "red"


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


renderLanes model =
    let
        originLeftXY =
            shift { startXY = model.roadCenterline.originXY, startCourseDeg = model.roadCenterline.originCourseDeg } (-model.laneWidthFeet / 2.0)

        originRightXY =
            shift { startXY = model.roadCenterline.originXY, startCourseDeg = model.roadCenterline.originCourseDeg } (model.laneWidthFeet / 2.0)

        originCourse =
            model.roadCenterline.originCourseDeg
    in
    recursiveRenderLanes model { startXY = originLeftXY, startCourseDeg = originCourse } { startXY = originRightXY, startCourseDeg = originCourse } model.roadCenterline.segments []


recursiveRenderLanes model originLeft originRight segmentsRemaining resultsSoFar =
    case segmentsRemaining of
        [] ->
            resultsSoFar

        first :: rest ->
            let
                ( leftLane, endXY1, endCourse1 ) =
                    generateLane first originLeft 0.0

                ( rightLane, endXY2, endCourse2 ) =
                    generateLane first originRight 0.0

                renderedLeft =
                    renderLane model leftLane "blue"

                renderedRight =
                    renderLane model rightLane "blue"
            in
            recursiveRenderLanes model { startXY = endXY1, startCourseDeg = endCourse1 } { startXY = endXY2, startCourseDeg = endCourse2 } rest (List.append [ renderedLeft, renderedRight ] resultsSoFar)


getProjection relativeOffsetFeet =
    if relativeOffsetFeet < 0 then
        ( abs relativeOffsetFeet, -90.0 )

    else
        ( relativeOffsetFeet, 90.0 )


shift origin relativeOffsetFeet =
    let
        ( relativeDistance, relativeCourseDeg ) =
            getProjection relativeOffsetFeet
    in
    projectPoint origin.startXY (origin.startCourseDeg + relativeCourseDeg) relativeDistance


generateLane segment origin relativeOffsetFeet =
    let
        offsetOrigin =
            shift origin relativeOffsetFeet

        ( endXY, endCourseDeg ) =
            determineEndConditions segment offsetOrigin origin.startCourseDeg
    in
    case segment of
        Arc radiusFeet angleChangeDeg ->
            ( RenderableArc offsetOrigin origin.startCourseDeg endXY endCourseDeg (getSweepFlag angleChangeDeg) radiusFeet, endXY, endCourseDeg )

        Linear lengthFeet courseDeg ->
            ( RenderableLine offsetOrigin origin.startCourseDeg endXY courseDeg, endXY, courseDeg )


renderLanes2 model colorStr =
    let
        entityLists =
            List.map (\r -> renderLane2 model r colorStr) model.lanes
    in
    List.concat entityLists


renderLane2 model lane colorStr =
    let
        entities =
            List.map (\s -> renderLaneSegment model s colorStr) lane.segments
    in
    entities


renderLaneSegment model laneSegment colorStr =
    case laneSegment.segmentType of
        LinearLane ->
            let
                v1 =
                    toViewCoords2 laneSegment.startXY model

                v2 =
                    toViewCoords2 laneSegment.endXY model

                entity =
                    toSvgLineWithColor v1 v2 colorStr
            in
            entity

        ArcLane radiusFeet angleChangeDeg ->
            let
                v1 =
                    toViewCoords2 laneSegment.startXY model

                v2 =
                    toViewCoords2 laneSegment.endXY model

                arcRadius =
                    scaleToView model radiusFeet

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
                        , stroke colorStr
                        , fill "white"
                        , strokeWidth "2"
                        , fillOpacity "0.0"
                        ]
                        []
            in
            entity


renderLane model renderable colorStr =
    case renderable of
        RenderableArc startXY startCourseDeg endXY endCourseDeg sweepFlag radiusFeet ->
            let
                v1 =
                    toViewCoords2 startXY model

                v2 =
                    toViewCoords2 endXY model

                arcRadius =
                    scaleToView model radiusFeet

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
            in
            entity

        RenderableLine startXY startCourseDeg endXY endCourseDeg ->
            let
                v1 =
                    toViewCoords2 startXY model

                v2 =
                    toViewCoords2 endXY model

                entity =
                    toSvgLineWithColor v1 v2 colorStr
            in
            entity


determineEndConditions segment startXY startCourseDeg =
    case segment of
        Arc lengthFeet angleChangeDeg ->
            let
                ( arcOriginXY, arcEndpointXY ) =
                    computeArcPoints startXY startCourseDeg angleChangeDeg lengthFeet
            in
            ( arcEndpointXY, startCourseDeg + angleChangeDeg )

        Linear lengthFeet courseDeg ->
            let
                dx =
                    lengthFeet * sin (degrees courseDeg)

                dy =
                    lengthFeet * cos (degrees courseDeg)

                nextXY =
                    ( Tuple.first startXY + dx, Tuple.second startXY + dy )
            in
            ( nextXY, courseDeg )



-- type alias RenderableArc =
--     { startXY : ( Float, Float )
--     , startCourseDeg : Float
--     , endXY : ( Float, Float )
--     , endCourseDeg : Float
--     , sweepFlag : Int
--     , radiusFeet : Float
--     }
--type alias RenderableLine =
--    { startXY : ( Float, Float )
--    , startCourseDeg : Float
--    , endXY : ( Float, Float )
--    , endCourseDeg : Float
--    }


renderRoad model =
    -- need to first compute road coordinates relative to the viewbox
    let
        upperLeftX =
            model.viewCenterX - model.viewWidthFeet / 2.0

        upperLeftY =
            model.viewCenterY + model.viewWidthFeet / 2.0

        entities =
            toSvg model ( upperLeftX, upperLeftY ) model.roadCenterline
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
